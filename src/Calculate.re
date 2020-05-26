open Types;

let angleTo = ((dx, dy)) => atan2(dy, dx);

let dist = ((dx, dy)) => {
  sqrt(dx *. dx +. dy *. dy);
};

let dpos = ((x1, y1), (x2, y2)) => {
  let dx = x2 -. x1;
  let dy = y2 -. y1;
  (dx, dy);
};

let toPos = ((x, y)) => {x, y};

let rotateAround = (point, center, theta) => {
  let diff = dpos(point, center);
  let angle = angleTo(diff);
  let mag = dist(diff);

  (
    fst(center) +. cos(theta +. angle) *. mag,
    snd(center) +. sin(theta +. angle) *. mag,
  );
};

// TODO this could infinite loop
// Need to check for that.
let rec resolvePoint = (id: reference, scene: scene, positions: positions) => {
  switch (id) {
  | Actual(id) => getOrCalculatePoint(id, scene, positions)
  | Virtual({symmetry, index}) =>
    switch (Belt.Map.String.get(scene.symmetries, symmetry)) {
    | None => raise(Not_found)
    | Some({center, point, count}) =>
      let center = resolvePoint(center, scene, positions);
      // let point = resolvePoint(point, scene, positions);
      let point = getOrCalculatePoint(point, scene, positions);
      let delta = dpos(center, point);
      let mag = dist(delta);
      let theta = angleTo(delta);
      let around =
        Js.Math._PI *. 2. /. float_of_int(count) *. float_of_int(index);
      (
        fst(center) +. cos(theta +. around) *. mag,
        snd(center) +. sin(theta +. around) *. mag,
      );
    }
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
  switch (point) {
  | Abs({x, y}) => (x, y)
  | Line({source, dest, percentOrAbs}) =>
    let (x1, y1) = resolvePoint(source, scene, positions);
    let (x2, y2) = resolvePoint(dest, scene, positions);
    let dx = x2 -. x1;
    let dy = y2 -. y1;
    switch (percentOrAbs) {
    | Percent(perc) => (x1 +. dx *. perc, y1 +. dy *. perc)
    // | Abs(v) => x1 +.
    };
  | Circle({center, onEdge, angle, offset}) =>
    let (cx, cy) = resolvePoint(center, scene, positions);
    let p = resolvePoint(onEdge, scene, positions);
    let delta = dpos((cx, cy), p);
    let mag = dist(delta);
    let theta = atan2(snd(delta), fst(delta)) +. angle;
    let Percent(offset) = offset;
    (cx +. cos(theta) *. mag *. offset, cy +. sin(theta) *. mag *. offset);
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

// Hmm this should include symmetry positions too I guess.
let calculateAllPositions = scene => {
  let positions = Hashtbl.create(Belt.Map.String.size(scene.points));
  scene.points
  ->Belt.Map.String.forEach((key, v) => {
      getOrCalculatePoint(key, scene, positions)->ignore
    });
  let symPoints =
    scene.symmetries
    ->S.toArray
    ->Belt.Array.map(((k, sym)) => {
        let res = [||];
        for (x in 1 to sym.count - 1) {
          res
          ->Js.Array2.push((
              k,
              resolvePoint(Api.Ref.sym(k, x), scene, positions),
            ))
          ->ignore;
        };
        res;
      })
    ->Belt.Array.concatMany;
  (positions, symPoints);
};

let symShapes = (scene, positions) => {
  scene.shapeSymmetries
  ->S.toArray
  ->Belt.Array.map(((k, sym)) => {
      let res = [||];
      for (x in 1 to sym.count - 1) {
        res
        ->Js.Array2.push((
            k,
            resolvePoint(Api.Ref.sym(k, x), scene, positions),
          ))
        ->ignore;
      };
      res;
    })
  ->Belt.Array.concatMany;
};

// let rotateShape = (shape: concreteShape, center: (float, float), theta: float) => {
//   switch shape {
//     | CLine({p1, p2})
//   }
// }

let shape = (shape: shapeKind, scene: scene, positions: positions) =>
  switch (shape) {
  | Line({p1, p2}) =>
    let (x1, y1) = resolvePoint(p1, scene, positions);
    let (x2, y2) = resolvePoint(p2, scene, positions);
    CLine({
      p1: {
        x: x1,
        y: y1,
      },
      p2: {
        x: x2,
        y: y2,
      },
    });
  | Circle({center, onEdge}) =>
    let (cx, cy) = resolvePoint(center, scene, positions);
    let onEdge = resolvePoint(onEdge, scene, positions);

    let r = dist(dpos((cx, cy), onEdge));
    CCircle({
      center: {
        x: cx,
        y: cy,
      },
      r,
    });
  };

let line = (p1, p2, scene, positions) => {
  let (x1, y1) = resolvePoint(p1, scene, positions);
  let (x2, y2) = resolvePoint(p2, scene, positions);
  (x1, y1, x2, y2);
};

let circle = (center, onEdge, scene, positions) => {
  let (cx, cy) = resolvePoint(center, scene, positions);
  let onEdge = resolvePoint(onEdge, scene, positions);

  let r = dist(dpos((cx, cy), onEdge));
  (cx, cy, r);
};