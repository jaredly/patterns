open Types;

let angleTo = ({x, y}) => atan2(y, x);

let dist = ({x, y}) => {
  sqrt(x *. x +. y *. y);
};

let dpos = (p1, p2) => {
  let x = p2.x -. p1.x;
  let y = p2.y -. p1.y;
  {x, y};
};

let toPos = ((x, y)) => {x, y};

let rotateAround = (point, center, theta) => {
  let diff = dpos(center, point);
  let angle = angleTo(diff);
  let mag = dist(diff);

  {
    x: center.x +. cos(theta +. angle) *. mag,
    y: center.y +. sin(theta +. angle) *. mag,
  };
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
  let ratio = ar /. (ar +. br);
  let diff = dpos(ac, bc);
  let dist = dist(diff);
  let middle = {x: ac.x +. diff.x *. ratio, y: ac.y +. diff.y *. ratio};
  if (dist > ar +. br) {
    [];
  } else if (dist == ar *. br) {
    [middle];
  } else {
    let a = (ar *. ar -. br *. br +. dist *. dist) /. (2. *. dist);
    let h = sqrt(ar *. ar -. a *. a);
    [
      {
        x: middle.x +. h *. (bc.y -. ac.y) /. dist,
        y: middle.y -. h *. (bc.x -. ac.x) /. dist,
      },
      {
        x: middle.x -. h *. (bc.y -. ac.y) /. dist,
        y: middle.y +. h *. (bc.x -. ac.x) /. dist,
      },
    ];
  };
};

let intersection = (ap1, ap2, bp1, bp2) => {
  let (m1, b1) = generic(ap1, ap2);
  let (m2, b2) = generic(bp1, bp2);
  if (m1 == m2) {
    None;
  } else {
    let x = (b2 -. b1) /. (m1 -. m2);
    let y = m1 *. x +. b1;
    Some({x, y});
  };
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
  // switch (id) {
  // | Actual(id) => getOrCalculatePoint(id, scene, positions)
  // | Virtual({symmetry, index}) =>
  //   switch (Belt.Map.String.get(scene.symmetries, symmetry)) {
  //   | None => raise(Not_found)
  //   | Some({center, point, count}) =>
  //     let center = resolvePoint(center, scene, positions);
  //     // let point = resolvePoint(point, scene, positions);
  //     let point = getOrCalculatePoint(point, scene, positions);
  //     let delta = dpos(center, point);
  //     let mag = dist(delta);
  //     let theta = angleTo(delta);
  //     let around =
  //       Js.Math._PI *. 2. /. float_of_int(count) *. float_of_int(index);
  //     {
  //       x: center.x +. cos(theta +. around) *. mag,
  //       y: center.y +. sin(theta +. around) *. mag,
  //     };
  //   }
  // };
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

let shape = (shape: shapeKind, scene: scene, positions: positions) =>
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
    CCirclePart({center: c, r, theta0, theta1});
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
  | CCirclePart({center: ccenter, r, theta0, theta1}) =>
    CCirclePart({
      center: rotateAround(ccenter, center, theta),
      r,
      theta0: theta0 +. theta,
      theta1: theta1 +. theta,
    })
  };
};

let resolveShape = (scene, ref, positions) => {
  let {kind, sym} = scene.shapes->S.getExn(ref.id);
  let base = shape(kind, scene, positions);
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

let calculateShapes = (scene, positions) => {
  scene.shapes
  ->S.toArray
  ->Belt.Array.map(((k, {kind, sym, color})) => {
      let one = shape(kind, scene, positions);
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
  // let shapesMap = scene.shapes->S.map(s => shape(s, scene, positions));
  // let syms =
  //   scene.shapeSymmetries
  //   ->S.toArray
  //   ->Belt.Array.map(((k, sym)) => {
  //       let shape = shapesMap->S.getExn(sym.shape);
  //       let res = [||];
  //       let by = Js.Math._PI *. 2. /. float_of_int(sym.count);
  //       for (x in 1 to sym.count - 1) {
  //         res
  //         ->Js.Array2.push((
  //             (k, x),
  //             rotateShape(
  //               shape,
  //               resolvePoint(sym.center, scene, positions),
  //               by *. float_of_int(x),
  //               // 0.,
  //             ),
  //           ))
  //         ->ignore;
  //       };
  //       res;
  //     })
  //   ->Belt.Array.concatMany;
  // (shapesMap->S.toArray, syms);
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
  (positions, points, shapes);
};

/**  ok */;
// let line = (p1, p2, scene, positions) => {
//   let p1 = resolvePoint(p1, scene, positions);
//   let p2 = resolvePoint(p2, scene, positions);
//   (x1, y1, x2, y2);
// };

// let circle = (center, onEdge, scene, positions) => {
//   let (cx, cy) = resolvePoint(center, scene, positions);
//   let onEdge = resolvePoint(onEdge, scene, positions);

//   let r = dist(dpos((cx, cy), onEdge));
//   (cx, cy, r);
// };
/* ok */