use crate::*;

#[derive(Debug, Clone, Copy, Default)]
pub struct RaycastResult {
	pub impact: Option<RaycastImpact>,
	/// The index of the leaf the ray ended up in.
	pub leaf_idx: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct RaycastImpact {
	pub fraction: f32,
	pub position: Vec3,
	pub normal: Vec3,

	/// The index of the node containing the surface that was hit.
	pub node_idx: u32,
	// TODO Perhaps we also want to return the face it impacted?
}

impl BspData {
	/// Returns the index of the BSP leaf `point` is in of `model`.
	pub fn leaf_at_point(&self, model_idx: usize, point: Vec3) -> usize {
		self.leaf_at_point_in_node(self.models[model_idx].head_bsp_node, point)
	}

	/// Returns the index of the BSP leaf `point` is in inside a specific node. Usually, you probably want [`Self::leaf_at_point`].
	pub fn leaf_at_point_in_node(&self, mut node_ref: BspNodeRef, point: Vec3) -> usize {
		loop {
			match node_ref {
				BspNodeRef::Leaf(leaf_idx) => return leaf_idx as usize,
				BspNodeRef::Node(node_idx) => {
					let node = &self.nodes[node_idx as usize];
					let plane = &self.planes[node.plane_idx as usize];

					if plane.point_side(point) >= 0. {
						node_ref = node.front.0;
					} else {
						node_ref = node.back.0;
					}
				}
			}
		}
	}

	/// Implementation of Quake's `SV_RecursiveHullCheck` function.
	/// Traces a line through this data's model at `model_idx`, returning information of the leaf it ends up at, and the first surface it hits if any.
	///
	/// TODO There are more numerically stable implementations, but i couldn't get them to work. I guess if it works for Quake it works for me!
	pub fn raycast(&self, model_idx: usize, from: Vec3, to: Vec3) -> RaycastResult {
		if from == to {
			return RaycastResult {
				impact: None,
				leaf_idx: self.leaf_at_point(model_idx, from),
			};
		}

		const DIST_EPSILON: f32 = 0.03125;

		let model = self.models[model_idx];

		struct Ctx<'a> {
			start: Vec3,
			end: Vec3,
			data: &'a BspData,
		}

		fn internal(ctx: &Ctx, mut node_ref: BspNodeRef, from: Vec3, to: Vec3) -> RaycastResult {
			// We're using this loop as a Rust `goto`.
			'reenter: loop {
				let node_idx = match node_ref {
					BspNodeRef::Leaf(leaf_idx) => {
						return RaycastResult {
							impact: None,
							leaf_idx: leaf_idx as usize,
						};
					}
					BspNodeRef::Node(node_idx) => node_idx,
				};

				let node = &ctx.data.nodes[node_idx as usize];
				let plane = &ctx.data.planes[node.plane_idx as usize];

				let from_dist = plane.point_side(from);
				let to_dist = plane.point_side(to);

				// Close the area around the ray.
				if from_dist >= 0. && to_dist >= 0. {
					node_ref = node.front.0;
					continue 'reenter;
				} else if from_dist < 0. && to_dist < 0. {
					node_ref = node.back.0;
					continue 'reenter;
				}

				// Points lie on different sides of the plane.

				// The side containing `from`
				let front_side = from_dist >= 0.;

				// The fraction needed to get to the plane.
				let frac = if front_side { from_dist + DIST_EPSILON } else { from_dist - DIST_EPSILON } / (from_dist - to_dist);
				let mid = from.lerp(to, frac);

				// Trace through child nodes near child first
				let side_result = internal(ctx, if front_side { node.front.0 } else { node.back.0 }, from, mid);
				if ctx.data.leaves[side_result.leaf_idx].contents == BspLeafContents::Solid {
					return side_result;
				}

				// Sort of hacky, but this is what Quake's implementation does, so i guess it's fine.
				let mid_leaf_idx = ctx.data.leaf_at_point_in_node(if front_side { node.back.0 } else { node.front.0 }, mid);
				if ctx.data.leaves[mid_leaf_idx].contents != BspLeafContents::Solid {
					return internal(ctx, if front_side { node.back.0 } else { node.front.0 }, mid, to);
				}

				// The contents at mid are solid, we hit something!

				// Calculate mid without DIST_EPSILON
				let real_mid = from.lerp(to, from_dist / (from_dist - to_dist));

				let impact = RaycastImpact {
					// Since we don't store the fraction, we have to calculate it here. Hopefully floating-point precision doesn't hurt too much from this.
					fraction: ((real_mid - ctx.start) / (ctx.end - ctx.start)).element_sum() / 3.,
					position: real_mid,
					normal: if front_side { -plane.normal } else { plane.normal },
					node_idx,
				};

				// The other side of the node is solid, this is the impact point
				return RaycastResult {
					impact: Some(impact),
					leaf_idx: mid_leaf_idx,
				};
			}
		}

		let ctx = Ctx {
			start: from,
			end: to,
			data: self,
		};

		internal(&ctx, model.head_bsp_node, from, to)
	}
}
