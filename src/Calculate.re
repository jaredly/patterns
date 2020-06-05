open Types;

let pi = Js.Math._PI;
let pi2 = pi /. 2.;
let tau = pi *. 2.;

let angleTo = ({x, y}) => atan2(y, x);

let dist = ({x, y}) => {
  sqrt(x *. x +. y *. y);
};

let dpos = (p1, p2) => {
  let x = p2.x -. p1.x;
  let y = p2.y -. p1.y;
  {x, y};
};

let addPos = (a, b) => {x: a.x +. b.x, y: a.y +. b.y};

let toPos = ((x, y)) => {x, y};

let scalePos = ({x, y}, scale) => {x: x *. scale, y: y *. scale};

let rotateAround = (point, center, theta) => {
  let diff = dpos(center, point);
  let angle = angleTo(diff);
  let mag = dist(diff);

  {
    x: center.x +. cos(theta +. angle) *. mag,
    y: center.y +. sin(theta +. angle) *. mag,
  };
};

let push = ({x, y}, ~theta, ~mag) => {
  x: x +. cos(theta) *. mag,
  y: y +. sin(theta) *. mag,
};

let flip = shape =>
  switch (shape) {
  | CLine({p1, p2}) => CLine({p1: p2, p2: p1})
  | CCircle(_) => shape
  | CCirclePart({center, r, theta0, theta1, clockwise}) =>
    CCirclePart({
      center,
      r,
      theta0: theta1,
      theta1: theta0,
      clockwise: !clockwise,
      // TODO verify this...
      // theta0: theta1 -. Js.Math._PI *. 2.,
      // theta1: theta0,
    })
  };

let endPoints = shape =>
  switch (shape) {
  | CLine({p1, p2}) => (p1, p2)
  | CCirclePart({center, r, theta0, theta1}) =>
    let start = {
      x: center.x +. cos(theta0) *. r,
      y: center.y +. sin(theta0) *. r,
    };
    let endd = {
      x: center.x +. cos(theta1) *. r,
      y: center.y +. sin(theta1) *. r,
    };
    (start, endd);
  | CCircle(_) => ({x: 0., y: 0.}, {x: 0., y: 0.})
  };

/*

 y = mx + b


 y = 1x + 1
 y = -0.5x + 4

 1x + 1 = -0.5x + 4

 (1 - -0.5) x = 4 - 1

 x = 3 / 1.5 = 4.5


 */

let generic = (p1, p2) => {
  let m = (p2.y -. p1.y) /. (p2.x -. p1.x);
  let b = p1.y -. m *. p1.x;
  (m, b);
};

// algorithm based on this fantastic answer https://stackoverflow.com/a/3349134
let intersectCircles = (ac, ar, bc, br) => {
  let percentFromA = ar /. (ar +. br);
  let diff = dpos(ac, bc);
  let dist = dist(diff);
  let gap = dist -. (ar +. br);
  if (abs_float(gap) < 0.01) {
    let middle = {
      x: ac.x +. diff.x *. percentFromA,
      y: ac.y +. diff.y *. percentFromA,
    };
    [middle];
  } else if (gap > 0.) {
    [];
  } else {
    /*
     Ok, we've got a triangle
     C is between the two centers
     A is rA
     B is rB

     H is the height

     a + b = C

     H^2 + a^2 = A^2
     H^2 + (C - a)^2 = B^2

     H^2 = A^2 - a^2
     H^2 = B^2 - (C - a)^2

     A^2 - a^2 = B^2 - (C - a)^2
     A^2 - a^2 = B^2 - (C^2 - 2aC + a^2)
     A^2 - a^2 = B^2 - C^2 + 2aC - a^2
     A^2 = B^2 - C^2 + 2aC
     A^2 - B^2 + C^2 = 2aC
     (A^2 - B^2 + C^2) / 2C = a

     */
    let a = (ar *. ar -. br *. br +. dist *. dist) /. (2. *. dist);
    let h = sqrt(ar *. ar -. a *. a);
    let t = angleTo(diff);
    let middle = push(ac, ~theta=t, ~mag=a);

    [
      push(middle, ~theta=t +. pi2, ~mag=h),
      push(middle, ~theta=t -. pi2, ~mag=h),
    ];
  };
  // let ratio = ar /. (ar +. br);
  // let middle = {x: ac.x +. diff.x *. ratio, y: ac.y +. diff.y *. ratio};
  // if (dist > ar +. br) {
  //   [];
  // } else if (dist == ar *. br) {
  //   [middle];
  // } else {
  //   let a = (ar *. ar -. br *. br +. dist *. dist) /. (2. *. dist);
  //   let h = sqrt(ar *. ar -. a *. a);
  //   [
  //     {
  //       x: middle.x +. h *. (bc.y -. ac.y) /. dist,
  //       y: middle.y -. h *. (bc.x -. ac.x) /. dist,
  //     },
  //     {
  //       x: middle.x -. h *. (bc.y -. ac.y) /. dist,
  //       y: middle.y +. h *. (bc.x -. ac.x) /. dist,
  //     },
  //   ];
  // };
};

let intersection = (ap1, ap2, bp1, bp2) => {
  let (m1, b1) = generic(ap1, ap2);
  let av = abs_float(ap1.x -. ap2.x) < 0.001;
  let (m2, b2) = generic(bp1, bp2);
  let bv = abs_float(bp1.x -. bp2.x) < 0.001;
  if (av && bv) {
    None;
  } else if (av) {
    Some({x: ap2.x, y: ap2.x *. m2 +. b2});
  } else if (bv) {
    Some({x: bp1.x, y: bp1.x *. m1 +. b1});
  } else if (m1 == m2) {
    None;
  } else {
    let x = (b2 -. b1) /. (m1 -. m2);
    let y = m1 *. x +. b1;
    Some({x, y});
  };
};

let force = x =>
  switch (x) {
  | None => failwith("unwrapped empty")
  | Some(x) => x
  };

let lineCircle = (center, r, p1, p2) => {
  let angle = angleTo(dpos(p1, p2));
  let closestPoint =
    intersection(p1, p2, center, push(center, ~theta=angle +. pi2, ~mag=5.))
    ->force;

  let dist = dist(dpos(closestPoint, center));
  if (dist > r) {
    [];
  } else if (abs_float(dist -. r) < 0.001) {
    [closestPoint];
  } else {
    let otherSide = sqrt(r *. r -. dist *. dist);
    [
      push(closestPoint, ~theta=angle, ~mag=otherSide),
      closestPoint,
      push(closestPoint, ~theta=angle, ~mag=-. otherSide),
    ];
  };
  // TODO vertical line tho
  // let (m, b) = generic(p1, p2);
  // double r, a, b, c; // given as input
  // double x0 = -a*c/(a*a+b*b), y0 = -b*c/(a*a+b*b);
  // if (c*c > r*r*(a*a+b*b)+EPS)
  //     puts ("no points");
  // else if (abs (c*c - r*r*(a*a+b*b)) < EPS) {
  //     puts ("1 point");
  //     cout << x0 << ' ' << y0 << '\n';
  // }
  // else {
  //     double d = r*r - c*c/(a*a+b*b);
  //     double mult = sqrt (d / (a*a+b*b));
  //     double ax, ay, bx, by;
  //     ax = x0 + b * mult;
  //     bx = x0 - b * mult;
  //     ay = y0 - a * mult;
  //     by = y0 + a * mult;
  //     puts ("2 points");
  //     cout << ax << ' ' << ay << '\n' << bx << ' ' << by << '\n';
  // }
};

// TODO this could infinite loop
// Need to check for that.
let rec resolvePoint = (id: reference, scene: scene, positions: positions) => {
  let base = getOrCalculatePoint(id.id, scene, positions);
  if (id.index == 0) {
    base;
  } else {
    let {pos: _, sym} = Belt.Map.String.getExn(scene.points, id.id);
    switch (sym) {
    | None => failwith("No symmetry")
    | Some({center, count}) =>
      let center = resolvePoint(center, scene, positions);
      let around =
        Js.Math._PI *. 2. /. float_of_int(count) *. float_of_int(id.index);
      rotateAround(base, center, around);
    };
  };
}

and getOrCalculatePoint = (id: string, scene: scene, positions: positions) => {
  switch (Hashtbl.find(positions, id)) {
  | exception Not_found =>
    switch (Belt.Map.String.get(scene.points, id)) {
    | None => raise(Not_found)
    | Some(point) =>
      let pos = calculatePoint(point, scene, positions);
      Hashtbl.replace(positions, id, pos);
      pos;
    }
  | pos => pos
  };
}

and calculatePoint = (point: point, scene: scene, positions: positions) => {
  switch (point.pos) {
  | Abs({x, y}) => {x, y}
  | Line({source, dest, percentOrAbs}) =>
    let p1 = resolvePoint(source, scene, positions);
    let p2 = resolvePoint(dest, scene, positions);
    let dx = p2.x -. p1.x;
    let dy = p2.y -. p1.y;
    switch (percentOrAbs) {
    | Percent(perc) => {x: p1.x +. dx *. perc, y: p1.y +. dy *. perc}
    // | Abs(v) => x1 +.
    };
  | RotateBetween({one, middle, two, amount}) =>
    let one = resolvePoint(one, scene, positions);
    let middle = resolvePoint(middle, scene, positions);
    let two = resolvePoint(two, scene, positions);
    let t1 = angleTo(dpos(middle, one));
    let t2 = angleTo(dpos(middle, two));
    let diff = t2 -. t1;
    let diff =
      if (abs_float(diff) > Js.Math._PI) {
        -. (Js.Math._PI *. 2. -. diff);
      } else {
        diff;
      };
    rotateAround(one, middle, diff *. amount);

  | Rotate({source, dest, theta}) =>
    let p1 = resolvePoint(source, scene, positions);
    let p2 = resolvePoint(dest, scene, positions);
    rotateAround(p1, p2, theta);
  | Circle({center, onEdge, angle, offset}) =>
    let c = resolvePoint(center, scene, positions);
    let p = resolvePoint(onEdge, scene, positions);
    let delta = dpos(c, p);
    let mag = dist(delta);
    let theta = atan2(delta.y, delta.x) +. angle;
    let Percent(offset) = offset;
    {
      x: c.x +. cos(theta) *. mag *. offset,
      y: c.y +. sin(theta) *. mag *. offset,
    };
  // | Radial({center, point, count, index}) =>
  //   let (cx, cy) = resolvePoint(center, scene, positions);
  //   let p = resolvePoint(point, scene, positions);
  //   let delta = dpos((cx, cy), p);
  //   let mag = dist(delta);
  //   let angle =
  //     Js.Math._PI *. 2. /. float_of_int(count) *. float_of_int(index);
  //   let theta = atan2(snd(delta), fst(delta)) +. angle;
  //   (cx +. cos(theta) *. mag, cy +. sin(theta) *. mag);
  };
};

module S = Belt.Map.String;

let calculateShape = (shape: shapeKind, scene: scene, positions: positions) =>
  switch (shape) {
  | Line({p1, p2}) =>
    let p1 = resolvePoint(p1, scene, positions);
    let p2 = resolvePoint(p2, scene, positions);
    CLine({p1, p2});
  | Circle({center, onEdge}) =>
    let c = resolvePoint(center, scene, positions);
    let onEdge = resolvePoint(onEdge, scene, positions);

    let r = dist(dpos(c, onEdge));
    CCircle({center: c, r});
  | CirclePart({center, onEdge, goUntil}) =>
    let c = resolvePoint(center, scene, positions);
    let onEdge = resolvePoint(onEdge, scene, positions);
    let goUntil = resolvePoint(goUntil, scene, positions);

    let r = dist(dpos(c, onEdge));
    let theta0 = angleTo(dpos(c, onEdge));
    let theta1 = angleTo(dpos(c, goUntil));
    CCirclePart({center: c, r, theta0, theta1, clockwise: true});
  };

let rotateShape = (shape: concreteShape, center: pos, theta: float) => {
  switch (shape) {
  | CLine({p1, p2}) =>
    CLine({
      p1: rotateAround(p1, center, theta),
      p2: rotateAround(p2, center, theta),
    })
  | CCircle({center: ccenter, r}) =>
    CCircle({center: rotateAround(ccenter, center, theta), r})
  | CCirclePart({center: ccenter, r, theta0, theta1, clockwise}) =>
    CCirclePart({
      center: rotateAround(ccenter, center, theta),
      r,
      theta0: theta0 +. theta,
      theta1: theta1 +. theta,
      clockwise,
    })
  };
};

let resolveShape = (scene, ref, positions) => {
  let {kind, sym} = scene.shapes->S.getExn(ref.id);
  let base = calculateShape(kind, scene, positions);
  if (ref.index == 0) {
    base;
  } else {
    switch (sym) {
    | None => failwith("no symmetry")
    | Some(sym) =>
      let by = Js.Math._PI *. 2. /. float_of_int(sym.count);
      rotateShape(
        base,
        resolvePoint(sym.center, scene, positions),
        by *. float_of_int(ref.index),
      );
    };
  };
};

let tile = (sides, scene, positions) => {
  sides->Belt.List.map(r => resolveShape(scene, r, positions));
};

let calculateTiles = (scene, positions) => {
  scene.tiles
  ->S.toArray
  ->Belt.Array.map(((k, t)) => {
      let base = tile(t.sides, scene, positions);
      switch (t.sym) {
      | None => [|({id: k, index: 0}, base, t)|]
      | Some(sym) =>
        let res = [|({id: k, index: 0}, base, t)|];
        let by = Js.Math._PI *. 2. /. float_of_int(sym.count);
        for (x in 1 to sym.count - 1) {
          res
          ->Js.Array2.push((
              {id: k, index: x},
              base->Belt.List.map(one =>
                rotateShape(
                  one,
                  resolvePoint(sym.center, scene, positions),
                  by *. float_of_int(x),
                )
              ),
              t,
            ))
          ->ignore;
        };
        res;
      };
    })
  ->Belt.Array.concatMany;
};

let calculateShapes = (scene, positions) => {
  scene.shapes
  ->S.toArray
  ->Belt.Array.map(((k, {kind, sym, color})) => {
      let one = calculateShape(kind, scene, positions);
      switch (sym) {
      | None => [|({id: k, index: 0}, one, color)|]

      | Some(sym) =>
        let res = [|({id: k, index: 0}, one, color)|];
        let by = Js.Math._PI *. 2. /. float_of_int(sym.count);
        for (x in 1 to sym.count - 1) {
          res
          ->Js.Array2.push((
              {id: k, index: x},
              rotateShape(
                one,
                resolvePoint(sym.center, scene, positions),
                by *. float_of_int(x),
                // 0.,
              ),
              color,
            ))
          ->ignore;
        };
        res;
      };
    })
  ->Belt.Array.concatMany;
};

// Hmm this should include symmetry positions too I guess.
let calculateAllPositions = scene => {
  let positions = Hashtbl.create(Belt.Map.String.size(scene.points));
  let points =
    scene.points
    ->Belt.Map.String.toArray
    ->Belt.Array.map(((key, v)) => {
        let one = getOrCalculatePoint(key, scene, positions);
        switch (v.sym) {
        | None => [|({id: key, index: 0}, one)|]
        | Some(sym) =>
          let res = [|({id: key, index: 0}, one)|];
          for (x in 1 to sym.count - 1) {
            res
            ->Js.Array2.push((
                {id: key, index: x},
                resolvePoint({id: key, index: x}, scene, positions),
              ))
            ->ignore;
          };
          res;
        };
      })
    ->Belt.Array.concatMany;
  let shapes = calculateShapes(scene, positions);
  (positions, points, shapes, calculateTiles(scene, positions));
};

let toDegrees = rads => rads /. Js.Math._PI *. 180.;

let translatePos = (pos, rel) => {
  switch (pos) {
  | Abs({x, y}) => Abs({x: x +. rel.x, y: y +. rel.y})
  | pos => pos
  };
};

let translateEverything = (scene: scene, rel) => {
  {
    ...scene,
    points:
      scene.points
      ->Belt.Map.String.map(point =>
          {...point, pos: translatePos(point.pos, rel)}
        ),
  };
};

let bestSym = (sym, sym2) =>
  switch (sym, sym2) {
  | (None, Some(x)) => Some(x)
  | (Some(x), None) => Some(x)
  | (Some(a), Some(b)) => a.count > b.count ? Some(b) : Some(a)
  | _ => None
  };

let inCenter = (a1, a2, b1, b2, c1, c2) => {
  switch (
    intersection(a1, a2, b1, b2),
    intersection(a1, a2, c1, c2),
    intersection(b1, b2, c1, c2),
  ) {
  | (Some(i0), Some(i1), Some(i2)) =>
    let t01 = angleTo(dpos(i0, i1));
    let t02 = angleTo(dpos(i0, i2));
    let t0 = t01 +. (t02 -. t01) /. 2.;
    let t10 = angleTo(dpos(i1, i0));
    let t12 = angleTo(dpos(i1, i2));
    let t1 = t10 +. (t12 -. t10) /. 2.;

    intersection(
      i0,
      push(i0, ~theta=t0, ~mag=5.),
      i1,
      push(i1, ~theta=t1, ~mag=5.),
    );
  | _ => None
  };
};

let circumCenter = (a1, a2, b1, b2, c1, c2) => {
  switch (
    intersection(a1, a2, b1, b2),
    intersection(a1, a2, c1, c2),
    intersection(b1, b2, c1, c2),
  ) {
  | (Some(i0), Some(i1), Some(i2)) =>
    let m0 = addPos(i0, dpos(i0, i1)->scalePos(0.5));
    let t0 = angleTo(dpos(i0, i1));
    let m1 = addPos(i0, dpos(i0, i2)->scalePos(0.5));
    let t1 = angleTo(dpos(i0, i2));
    let m0a = push(m0, ~theta=t0 +. pi2, ~mag=5.);
    let m1a = push(m1, ~theta=t1 +. pi2, ~mag=5.);
    intersection(m0, m0a, m1, m1a);
  | _ => None
  };
};