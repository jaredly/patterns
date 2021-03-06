open Types;
open Calculate;

let findMap = (arr, fn) => {
  let rec loop = i =>
    if (i >= Array.length(arr)) {
      None;
    } else {
      let item = arr[i];
      switch (fn(item)) {
      | None => loop(i + 1)
      | Some(v) => Some((v, i))
      };
    };
  loop(0);
};

let orderItems = items => {
  let pool = Belt.List.toArray(items);
  let ordered = [||];

  /**
   * how to sort things?
   * pick one.
   * go through until you fine the one at its tail
   * flip that if needed
   * keep going
   */
  let rec loop = endp =>
    if (Array.length(pool) == 0) {
      ();
    } else {
      let found =
        pool->findMap(shape => {
          let (astartp, aendp) = Calculate.endPoints(shape);
          if (almostEqual(astartp, endp)) {
            Some((shape, aendp));
          } else if (almostEqual(aendp, endp)) {
            Some((Calculate.flip(shape), astartp));
          } else {
            None;
          };
        });
      switch (found) {
      | None => ()
      | Some(((shape, endp), idx)) =>
        pool->Js.Array2.spliceInPlace(~pos=idx, ~remove=1, ~add=[||])->ignore;
        ordered->Js.Array2.push(shape)->ignore;
        loop(endp);
      };
    };

  let first = pool->Js.Array2.pop->force;
  let (_, endp) = Calculate.endPoints(first);
  ordered->Js.Array2.push(first)->ignore;
  loop(endp);

  ordered;
};

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

let angleBetween = (a, b, clockwise) => {
  // now both are between 0 and Tau
  let a = normalizeTheta(a);
  let b = normalizeTheta(b);
  if (clockwise) {
    if (a > b) {
      b +. Calculate.tau -. a;
    } else {
      b -. a;
    };
  } else if (b > a) {
    b -. a -. Calculate.tau;
  } else {
    b -. a;
  };
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
          let span = angleBetween(theta0, theta1, clockwise);
          let thetaMid = theta0 +. span /. 2.;
          let p1 = push(center, ~theta=thetaMid, ~mag=r);
          let p2 = push(center, ~theta=theta1, ~mag=r);
          [|(p0, p1), (p1, p2)|];
        | CCircle(_) => assert(false)
        }
      )
    ->Belt.Array.concatMany;
  let angles =
    endPoints->Belt.Array.map(((p1, p2)) => angleTo(dpos(p1, p2)));
  let diffs =
    angles->Belt.Array.mapWithIndex((i, angle) => {
      let prev = angles[i == 0 ? ln - 1 : i - 1];
      insideAngle(angle -. prev);
    });
  let totalWind = diffs->Belt.Array.reduce(0., (+.));
  // Js.log2("Total", totalWind);
  (endPoints, totalWind > 0.);
};

let xor = (a, b) => a ? b ? false : true : b;

/**
 * Find the point between the end of "prev" and the start of "next". For lines,
 * there's only ever one point of intersection, but if we've got arcs or circles
 * involved, there are two and we need to know which one.
 */
let collideEndToEnd = (prev, next, clockwise) => {
  switch (prev, next) {
  | (CLine(l1), CLine(l2)) =>
    if (almostEqual(l1.p2, l2.p1)) {
      Some(l1.p2);
    } else {
      intersection(l1.p1, l1.p2, l2.p1, l2.p2);
    }
  | (CCirclePart(c1), CCirclePart(c2)) =>
    if (almostEqual(c1.center, c2.center) && almostEqualf(c1.r, c2.r)) {
      Some(Calculate.push(c1.center, ~theta=c1.theta1, ~mag=c1.r));
    } else {
      switch (intersectCircles(c1.center, c1.r, c2.center, c2.r)) {
      | [p0, p1] =>
        let angleDiff = c1.theta1 -. c2.theta0;
        Some(normalizeTheta(angleDiff) > 0. ? p1 : p0);
      | _ => None
      };
    }
  | (CLine(l1), CCirclePart({center, r} as c1)) =>
    let points = lineCircle(center, r, l1.p1, l1.p2);
    switch (points) {
    | [] => None
    | [p] => Some(p)
    | [p1, _, p2] => Some(xor(clockwise, c1.clockwise) ? p2 : p1)
    | _ => None
    };
  | (CCirclePart({center, r} as c1), CLine(l1)) =>
    let points = lineCircle(center, r, l1.p1, l1.p2);
    switch (points) {
    | [] => None
    | [p] => Some(p)
    | [p1, _, p2] => Some(xor(clockwise, c1.clockwise) ? p1 : p2)
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
      }
    });
  items->List.rev->Belt.List.toArray;
};

let inset = (ordered, margin, debug) => {
  let ln = Array.length(ordered);
  // if we're clockwise, each line wants get pushed to
  // the right (if it's going up)
  // so, theta +. pi /. 2.
  /// Otherwise, push it theta -. pi /. 2.
  let (endPoints, clockwise) = getWind(ordered);
  if (debug) {
    endPoints->Belt.Array.map(((p1, p2)) => CLine({p1, p2}));
  } else {
    // let clockwise = wind < pi;
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
        | CCirclePart(c1) =>
          let dir = xor(clockwise, c1.clockwise) ? 1. : (-1.);
          // let dir =
          //   clockwise ? c1.clockwise ? (-1.) : 1. : c1.clockwise ? 1. : (-1.);
          CCirclePart({
            ...c1,
            r:
              // are we inside? or are we dancer?
              // I don't know .. what the deal is ..
              // How do I know what side of the road we're on...
              c1.r +. margin *. dir,
          });
        | x => x
        }
      );
    let clipped =
      pushed
      ->Belt.Array.mapWithIndex((i, shape) => {
          let prev =
            collideEndToEnd(
              pushed[i == 0 ? ln - 1 : i - 1],
              shape,
              clockwise,
            );
          let next =
            collideEndToEnd(
              shape,
              pushed[i == ln - 1 ? 0 : i + 1],
              clockwise,
            );
          switch (prev, next) {
          | (Some(prev), Some(next)) =>
            switch (shape) {
            | CLine(_) => Some(CLine({p1: prev, p2: next}))
            | CCirclePart(c1) =>
              let theta0 = angleTo(dpos(c1.center, prev));
              let theta1 = angleTo(dpos(c1.center, next));

              Some(CCirclePart({...c1, theta0, theta1}));
            | _ => None
            }
          | _ => None
          };
        })
      ->Belt.Array.keepMap(x => x);
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
    // Js.log(
    //   ordered->Belt.Array.map(Types.showConcreteShape)
    //   |> Array.to_list
    //   |> String.concat(" -> "),
    // );
    // Js.log(
    //   pushed->Belt.Array.map(Types.showConcreteShape)
    //   |> Array.to_list
    //   |> String.concat(" -> "),
    // );
    // Js.log(
    //   clipped->Belt.Array.map(Types.showConcreteShape)
    //   |> Array.to_list
    //   |> String.concat(" -> "),
    // );
    clipped;
  };
};