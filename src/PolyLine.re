open Types;
open Calculate;

let insideAngle = angle =>
  if (angle > Js.Math._PI) {
    angle -. Js.Math._PI *. 2.;
  } else if (angle < -. Js.Math._PI) {
    angle +. Js.Math._PI *. 2.;
  } else {
    angle;
  };

let normalizeTheta = t => t < -. pi ? t +. tau : t > pi ? t -. tau : t;

let angleDiff = (one, two) => {
  abs_float(normalizeTheta(one) -. normalizeTheta(two));
};

/** Determine if we're going clockwise or counterclockwise */
let getWind = ordered => {
  let ln = Array.length(ordered);
  let endPoints =
    ordered
    ->Belt.Array.map(shape =>
        switch (shape) {
        | CLine({p1, p2}) => [|(p1, p2)|]
        | CCirclePart({center, r, theta0, theta1, clockwise}) =>
          let p0 = push(center, ~theta=theta0, ~mag=r);
          let off = clockwise ? pi /. 50. : -. pi /. 50.;
          let p1 = push(center, ~theta=theta0 +. off, ~mag=10.);
          let p2 = push(center, ~theta=theta1 -. off, ~mag=10.);
          let p3 = push(center, ~theta=theta1, ~mag=r);
          [|(p0, p1), (p1, p2), (p2, p3)|];
        // [|(p0, p3)|];
        | CCircle(_) => assert(false)
        }
      )
    ->Belt.Array.concatMany;
  let angles =
    endPoints->Belt.Array.map(((p1, p2)) => angleTo(dpos(p1, p2)));
  // let angles =
  //   ordered->Belt.Array.map(shape => {
  //     switch (shape) {
  //     | CLine({p1, p2}) =>
  //       let t = angleTo(dpos(p1, p2));
  //       (t, t);
  //     | CCirclePart({center, r, theta0, theta1, clockwise}) =>
  //       // ooh. Hm. The clockwise bit.
  //       let midT = (theta0 +. theta1) /. 2.;
  //       let mid = push(center, ~theta=midT, ~mag=r);
  //       let p0 = push(center, ~theta=theta0, ~mag=r);
  //       let p1 = push(center, ~theta=theta1, ~mag=r);
  //       (angleTo(dpos(p0, mid)), angleTo(dpos(mid, p1)))
  //     // TODO maybe use a polymorphic variant?
  //     | CCircle(_) => assert(false)
  //     }
  //   });
  let diffs =
    angles->Belt.Array.mapWithIndex((i, angle) => {
      let prev = angles[i == 0 ? ln - 1 : i - 1];
      insideAngle(angle -. prev);
    });
  let totalWind = diffs->Belt.Array.reduce(0., (+.));
  totalWind;
};

/** Find the point between the end of "prev" and the start of "next". For lines,
 * there's only ever one point of intersection, but if we've got arcs or circles
 * involved, there are two and we need to know which one.
 */
let collideEndToEnd = (prev, next) => {
  switch (prev, next) {
  | (CLine(l1), CLine(l2)) => intersection(l1.p1, l1.p2, l2.p1, l2.p2)
  | (CLine(l1), CCirclePart({center, r, theta0})) =>
    let points = lineCircle(center, r, l1.p1, l1.p2);
    switch (points) {
    | [] => None
    | [p] => Some(p)
    | [p1, p2] =>
      let t1 = angleTo(dpos(center, p1));
      let t2 = angleTo(dpos(center, p2));
      if (angleDiff(theta0, t1) < angleDiff(theta0, t2)) {
        Some(p1);
      } else {
        Some(p2);
      };
    | _ => None
    };
  | (CCirclePart({center, r, theta1}), CLine(l1)) =>
    let points = lineCircle(center, r, l1.p1, l1.p2);
    switch (points) {
    | [] => None
    | [p] => Some(p)
    | [p1, p2] =>
      let t1 = angleTo(dpos(center, p1));
      let t2 = angleTo(dpos(center, p2));
      if (angleDiff(theta1, t1) < angleDiff(theta1, t2)) {
        Some(p1);
      } else {
        Some(p2);
      };
    | _ => None
    };
  | _ => None
  };
};

let (|?) = (a, b) =>
  switch (a) {
  | None => b
  | Some(a) => a
  };

let joinAdjacentLineSegments = ordered => {
  // let res = [||]
  let last = ref(None);
  let (_, items) =
    ordered->Belt.Array.reduce((None, []), ((last, items), shape) => {
      switch (shape) {
      | CLine({p1, p2}) => (
          Some((p1, p2)),
          switch (last) {
          | None => [shape, ...items]
          | Some((pr1, pr2)) =>
            let t1 = angleTo(dpos(pr1, pr2));
            let t2 = angleTo(dpos(p1, p2));
            if (t1 == t2) {
              switch (items) {
              | [_one, ...rest] => [CLine({p1: pr1, p2}), ...rest]
              | _ => [shape, ...items]
              };
            } else {
              [shape, ...items];
            };
          },
        )
      | _ => (None, [shape, ...items])
      // let next = switch shape {
      //   | CLine({p1, p2}) => Some((p1, p2))
      //   | _ => None
      // }
      }
    });
  items->List.rev->Belt.List.toArray;
  // ordered->Belt.Array.forEach(shape => {
  // })
};

let inset = (ordered, margin) => {
  let ln = Array.length(ordered);
  // if we're clockwise, each line wants get pushed to
  // the right (if it's going up)
  // so, theta +. pi /. 2.
  /// Otherwise, push it theta -. pi /. 2.
  let clockwise = getWind(ordered) > 0.;
  let pushed =
    ordered->Belt.Array.map(shape =>
      switch (shape) {
      | CLine({p1, p2}) =>
        let theta =
          angleTo(dpos(p1, p2))
          +. Js.Math._PI
          /. 2.
          *. (clockwise ? 1. : (-1.));
        CLine({
          p1: push(p1, ~theta, ~mag=margin),
          p2: push(p2, ~theta, ~mag=margin),
        });
      // erg there's definitely a case where I want the radius to increase.
      // if the way we're going is the opposite of the clockwised-ness of the arc, I think.
      | CCirclePart(c1) => CCirclePart({...c1, r: c1.r -. margin})
      | x => x
      }
    );
  let clipped =
    pushed->Belt.Array.mapWithIndex((i, shape) => {
      let prev = collideEndToEnd(pushed[i == 0 ? ln - 1 : i - 1], shape);
      let next = collideEndToEnd(shape, pushed[i == ln - 1 ? 0 : i + 1]);
      switch (shape) {
      | CLine({p1, p2}) => CLine({p1: prev |? p1, p2: next |? p2})
      | CCirclePart(c1) =>
        let theta0 =
          switch (prev) {
          | Some(p) => angleTo(dpos(c1.center, p))
          | None => c1.theta0
          };
        let theta1 =
          switch (next) {
          | Some(p) => angleTo(dpos(c1.center, p))
          | None => c1.theta1
          };

        // ***********
        // START HERE
        // ***********
        // In order for this to work, I need to un-normalize this (it's currently normalized)
        // and the start wasn't normalized, so we're getting all weird.
        // e.g. if theta1 was larger than theta0, it needs to be after.

        // Sooo clooose -- it looks like collideEndToEnd is picking the wrong one in the one case.
        let theta1 =
          if (c1.theta1 > c1.theta0) {
            if (theta1 > theta0) {
              theta1;
            } else {
              theta1 +. tau;
            };
          } else if (theta1 > theta0) {
            theta1 -. tau;
          } else {
            theta1;
          };

        CCirclePart({...c1, theta0, theta1});
      | x => x
      };
    });
  // Js.log("reversed");
  // getWind(ordered->Belt.Array.reverse);
  // ok, gotta find out the winding direction.
  // like, are we going clockwise or counter-clockwise.
  // I think we can track the "curvature" (sum of diff between angles)
  // and it should add up to one or the other?
  // Belt.Array.mapWithIndex((i, shape) => {
  //   let prev = ordered[i == 0 ? ln - 1 : i - 1];
  //   let next = ordered[i == ln - 1 ? 0 : i + 1];

  // })
  // ordered;
  // pushed;
  clipped;
};