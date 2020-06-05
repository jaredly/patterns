open Types;
open Calculate;

module S = Belt.Map.String;

let potentials = (scene: scene, selection: selection, positions) =>
  (
    switch (selection) {
    | {points: [], tiles: [], shapes: []} => []
    | {points: [p0], shapes: [r]} =>
      let p0 = resolvePoint(p0, scene, positions);
      let shape = scene.shapes->S.getExn(r.id);
      let concrete = resolveShape(scene, r, positions);
      switch (concrete) {
      | CLine({p1, p2}) =>
        let t = Calculate.angleTo(Calculate.dpos(p1, p2));
        let p3 = Calculate.push(p0, ~theta=t +. Calculate.pi2, ~mag=10.);
        switch (Calculate.intersection(p0, p3, p1, p2)) {
        | None => []
        | Some(p) => [`Point({sym: shape.sym, pos: Abs({x: p.x, y: p.y})})]
        };
      | CCircle({center, r})
      | CCirclePart({center, r}) =>
        let d = dist(dpos(p0, center));
        if (d > r +. 0.001) {
          let t = angleTo(dpos(center, p0));
          let t0 = acos(r /. d);
          let p1 = push(center, ~theta=t +. t0, ~mag=r);
          let p2 = push(center, ~theta=t -. t0, ~mag=r);
          [
            `Point({sym: shape.sym, pos: Abs({x: p1.x, y: p1.y})}),
            `Point({sym: shape.sym, pos: Abs({x: p2.x, y: p2.y})}),
          ];
        } else {
          [];
        };
      };
    | {points: [p2, p1], tiles: [], shapes: []} =>
      let {pos: _, sym} = S.getExn(scene.points, p1.id);
      let {pos: _, sym: sym2} = S.getExn(scene.points, p2.id);
      let sym = bestSym(sym, sym2);
      [
        `Shape({kind: Line({p1, p2}), sym, color: None}),
        `Shape({kind: Circle({center: p1, onEdge: p2}), sym, color: None}),
        `Point({
          sym,
          pos: Line({source: p1, dest: p2, percentOrAbs: Percent(0.5)}),
        }),
        `Point({
          sym,
          pos: Line({source: p1, dest: p2, percentOrAbs: Percent(2.)}),
        }),
        `Point({sym, pos: Rotate({source: p1, dest: p2, theta: pi /. 2.})}),
      ];
    | {points: [p3, p2, p1], tiles: [], shapes: []} =>
      let {pos: _, sym} = S.getExn(scene.points, p1.id);
      let {pos: _, sym: sym2} = S.getExn(scene.points, p2.id);
      let {pos: _, sym: sym3} = S.getExn(scene.points, p3.id);
      let sym = bestSym(bestSym(sym, sym2), sym3);
      [
        `Point({
          sym,
          pos: RotateBetween({one: p1, middle: p2, two: p3, amount: 0.5}),
        }),
        `Shape({
          color: None,
          sym,
          kind: CirclePart({center: p2, onEdge: p1, goUntil: p3}),
        }),
      ];
    | {tiles: [], shapes, points: []} =>
      let found =
        shapes->Belt.List.map(r =>
          (
            r,
            scene.shapes->S.getExn(r.id),
            resolveShape(scene, r, positions),
          )
        );
      (
        List.length(found) > 1
          ? [
            {
              let sym =
                switch (found) {
                | [(_, {sym}, _), ..._] => sym
                | _ => None
                };

              `Tile({
                color: "red",
                margin: 2.,
                order: 0.,
                sides: shapes,
                sym,
              });
            },
          ]
          : []
      )
      @ (
        switch (found) {
        | [(_, shape, CLine({p1, p2}))] => [
            `Point({
              sym: shape.sym,
              pos:
                Abs({
                  x: p1.x +. (p2.x -. p1.x) /. 2.,
                  y: p1.y +. (p2.y -. p1.y) /. 2.,
                }),
            }),
          ]
        | [(_, s1, CLine(l1)), (_, s2, CLine(l2)), (_, s3, CLine(l3))] =>
          let sym = bestSym(bestSym(s1.sym, s2.sym), s3.sym);
          let inCenter =
            Calculate.inCenter(l1.p1, l1.p2, l2.p1, l2.p2, l3.p1, l3.p2);
          switch (inCenter) {
          | None => []
          | Some(p) => [`Point({sym, pos: Abs({x: p.x, y: p.y})})]
          };
        | [(_, s1, CLine(l1)), (_, s2, CLine(l2))] =>
          let cross = intersection(l1.p1, l1.p2, l2.p1, l2.p2);
          switch (cross) {
          | None => []
          | Some(cross) => [
              `Point({
                sym: bestSym(s1.sym, s2.sym),
                pos: Abs({x: cross.x, y: cross.y}),
              }),
            ]
          };
        | [(_, s1, CCircle(c1)), (_, s2, CCircle(c2))] =>
          let cross = intersectCircles(c1.center, c1.r, c2.center, c2.r);
          let sym = bestSym(s1.sym, s2.sym);
          cross->Belt.List.map(point => {
            `Point({sym, pos: Abs({x: point.x, y: point.y})})
          });
        | [(_, sl, CLine(l1)), (_, sc, CCircle(c1))]
        | [(_, sc, CCircle(c1)), (_, sl, CLine(l1))] =>
          let crosses = lineCircle(c1.center, c1.r, l1.p1, l1.p2);
          let sym = bestSym(sl.sym, sc.sym);
          crosses->Belt.List.map(point => {
            `Point({sym, pos: Abs({x: point.x, y: point.y})})
          });
        | _ => []
        }
      );
    | _ => []
    }
  )
  ->Belt.List.map(item =>
      switch (item) {
      | `Point(point) =>
        `Point((point, calculatePoint(point, scene, positions)))
      | `Shape(shape) =>
        `Shape((shape, calculateShape(shape.kind, scene, positions)))
      | `Tile(t) => `Tile((tile(t.sides, scene, positions), t))
      }
    );