open Types;

module TranslateEverything = {
  [@react.component]
  let make = (~scene, ~setScene) => {
    let (dx, setDx) = Hooks.useState(0);
    let (dy, setDy) = Hooks.useState(0);
    <div>
      <input
        type_="number"
        value={string_of_int(dx)}
        onChange={evt =>
          setDx(int_of_string(evt->ReactEvent.Form.target##value))
        }
      />
      <input
        type_="number"
        value={string_of_int(dy)}
        onChange={evt =>
          setDy(int_of_string(evt->ReactEvent.Form.target##value))
        }
      />
      <button
        onClick={_ =>
          setScene(
            Calculate.translateEverything(
              scene,
              {x: float_of_int(dx), y: float_of_int(dy)},
            ),
          )
        }>
        {React.string("Translate scene")}
      </button>
    </div>;
  };
};

let setColors = (setColor, shapes, color) =>
  shapes->Belt.List.forEach(r => setColor(r, color));

let buttonsForShapes = (shapes, scene, setSelection, setScene, setColor) => {
  let colors =
    shapes == []
      ? []
      : [
        ("Black", () => setColors(setColor, shapes, Some("#000"))),
        ("Gray", () => setColors(setColor, shapes, Some("#aaa"))),
        ("Light", () => setColors(setColor, shapes, Some("#ccc"))),
        ("Trace", () => setColors(setColor, shapes, Some("#eee"))),
        ("Fade", () => setColors(setColor, shapes, None)),
      ];
  let positions = Hashtbl.create(10);
  // let resolved =
  //   shapes->Belt.List.map(r => {
  //     (r, Calculate.resolveShape(scene, r, positions))
  //   });
  // colors
  // @ (
  //   List.length(shapes) > 2
  //   && resolved->Belt.List.every(((r, shape)) =>
  //        switch (shape) {
  //        | CCircle(_) => false
  //        | _ => true
  //        }
  //      )
  //     ? [
  //       (
  //         "Add tile",
  //         () => {
  //           let r = List.hd(shapes);
  //           let {kind: _, sym} = scene.shapes->Belt.Map.String.getExn(r.id);
  //           let (scene, id) = scene->Api.Tile.add(~sym, shapes);
  //           setScene(scene);
  //           setSelection(None);
  //         },
  //       ),
  //     ]
  //     : []
  // )
  // @ (
  //   switch (resolved) {
  //   | [] => []
  //   | [(s, _)] => [
  //       (
  //         "Remove shape",
  //         (
  //           () => {
  //             setSelection(None);
  //             setScene({
  //               ...scene,
  //               shapes: scene.shapes->Belt.Map.String.remove(s.Types.id),
  //             });
  //           }
  //         ),
  //       ),
  //     ]
  //   | [(s1, CCircle(l1)), (_, CCircle(l2))] => [
  //       (
  //         "Add points at intersections",
  //         (
  //           () => {
  //             let cross =
  //               Calculate.intersectCircles(l1.center, l1.r, l2.center, l2.r);
  //             let {sym} = scene.shapes->Belt.Map.String.getExn(s1.id);
  //             let (scene, sels) =
  //               cross->Belt.List.reduce(
  //                 (scene, []),
  //                 ((scene, sels), pos) => {
  //                   let (scene, id) =
  //                     scene->Api.Point.abs(~sym, pos.x, pos.y);
  //                   (scene, [{id, index: 0}, ...sels]);
  //                 },
  //               );
  //             setScene(scene);
  //             setSelection(Some(Points(sels)));
  //           }
  //         ),
  //       ),
  //     ]
  //   | [(s1, CLine(l1)), (_, CCircle(c1))]
  //   | [(_, CCircle(c1)), (s1, CLine(l1))] => [
  //       (
  //         "Add points at intersection",
  //         (
  //           () => {
  //             let crosses =
  //               Calculate.lineCircle(c1.center, c1.r, l1.p1, l1.p2);
  //             let {sym} = scene.shapes->Belt.Map.String.getExn(s1.id);
  //             let (scene, ids) =
  //               crosses->Belt.List.reduce(
  //                 (scene, []),
  //                 ((scene, ids), point) => {
  //                   let (scene, id) =
  //                     scene->Api.Point.abs(~sym, point.x, point.y);
  //                   (scene, [{id, index: 0}, ...ids]);
  //                 },
  //               );
  //             setScene(scene);
  //             setSelection(Some(Points(ids)));
  //           }
  //         ),
  //       ),
  //     ]
  //   | [(s1, CLine(l1)), (_, CLine(l2))] => [
  //       (
  //         "Add point at intersection",
  //         (
  //           () => {
  //             let cross = Calculate.intersection(l1.p1, l1.p2, l2.p1, l2.p2);
  //             switch (cross) {
  //             | None => ()
  //             | Some(cross) =>
  //               let {sym} = scene.shapes->Belt.Map.String.getExn(s1.id);
  //               let (scene, id) =
  //                 scene->Api.Point.abs(~sym, cross.x, cross.y);
  //               setScene(scene);
  //               setSelection(Some(Points([{id, index: 0}])));
  //             };
  //           }
  //         ),
  //       ),
  //     ]
  //   // | shapes
  //   //     when
  //   //       shapes->Belt.List.every(((_, kind)) =>
  //   //         switch (kind) {
  //   //         | CCircle(_) => false
  //   //         | _ => true
  //   //         }
  //   //       ) => [
  //   //     (
  //   //       "Fill shape",
  //   //       (
  //   //         () => {
  //   //           let {pos: _, sym} = Belt.Map.String.getExn(scene.points, p1.id);
  //   //           let (scene, id) = scene->Api.Shape.poly(~sym, shapes->Belt.List.map(((reference, _)) => {
  //   //             let {kind} = scene.shapes->Belt.Map.String.getExn(s1.id);
  //   //           }));
  //   //           setScene(scene);
  //   //           setSelection(Some(Shapes([{id, index: 0}])));
  //   //         }
  //   //       ),
  //   //     ),
  //   //   ]
  //   | _ => []
  //   }
  // );
  // | {shapes: [r], points: [], tiles: []} => [(
  //         "Remove shape",
  //         (
  //           () => {
  //             setSelection(None);
  //             setScene({
  //               ...scene,
  //               shapes: scene.shapes->Belt.Map.String.remove(s.Types.id),
  //             });
  //           }
  //         ),

  // )]
  colors;
};

// let buttonsForPoints = (points, scene, setSelection, setScene) => {
//   switch (points) {
//   | [_] => []
//   | [p1, p2] => [
//       (
//         "Add midpoint",
//         (
//           () => {
//             let {pos: _, sym} = Belt.Map.String.getExn(scene.points, p1.id);
//             // setSelection(None);
//             let (scene, id) = scene->Api.Point.line(~sym, p1, p2, 0.5);
//             setScene(scene);
//             setSelection(Some(Points([{id, index: 0}])));
//           }
//         ),
//       ),
//       (
//         "Add mirror point",
//         (
//           () => {
//             let {pos: _, sym} = Belt.Map.String.getExn(scene.points, p1.id);
//             // setSelection(None);
//             let (scene, id) = scene->Api.Point.line(~sym, p2, p1, 2.);
//             setScene(scene);
//             setSelection(Some(Points([{id, index: 0}])));
//           }
//         ),
//       ),
//       (
//         "Add circle",
//         (
//           () => {
//             let {pos: _, sym} = Belt.Map.String.getExn(scene.points, p1.id);
//             let (scene, id) = scene->Api.Shape.circle(~sym, p1, p2);
//             setScene(scene);
//             setSelection(Some(Shapes([{id, index: 0}])));
//           }
//         ),
//       ),
//       (
//         "Add line",
//         (
//           () => {
//             let {pos: _, sym} = Belt.Map.String.getExn(scene.points, p1.id);
//             let (scene, id) = scene->Api.Shape.line(~sym, p1, p2);
//             setScene(scene);
//             setSelection(Some(Shapes([{id, index: 0}])));
//           }
//         ),
//       ),
//       (
//         "Add perpendicular point",
//         (
//           () => {
//             let {pos: _, sym} = Belt.Map.String.getExn(scene.points, p2.id);
//             // setSelection(None);
//             let (scene, id) =
//               scene->Api.Point.rotate(~sym, p1, p2, Js.Math._PI /. 2.0);
//             setScene(scene);
//             setSelection(Some(Points([{id, index: 0}])));
//           }
//         ),
//       ),
//     ]
//   | [p1, p2, p3] => [
//       (
//         "Add circle part",
//         (
//           () => {
//             let {pos: _, sym} = Belt.Map.String.getExn(scene.points, p1.id);
//             let (scene, id) = scene->Api.Shape.circlePart(~sym, p3, p2, p1);
//             setScene(scene);
//             setSelection(Some(Shapes([{id, index: 0}])));
//           }
//         ),
//       ),
//       (
//         "Angle Bisector",
//         (
//           () => {
//             let {pos: _, sym} = Belt.Map.String.getExn(scene.points, p1.id);
//             // setSelection(None);
//             let (scene, id) =
//               scene->Api.Point.rotateBetween(~sym, p1, p2, p3, 0.5);
//             setScene(scene);
//             setSelection(Some(Points([{id, index: 0}])));
//           }
//         ),
//       ),
//       (
//         "Mirror across",
//         (
//           () => {
//             let {pos: _, sym} = Belt.Map.String.getExn(scene.points, p1.id);
//             // setSelection(None);
//             let (scene, id) =
//               scene->Api.Point.rotateBetween(~sym, p1, p2, p3, 2.);
//             setScene(scene);
//             setSelection(Some(Points([{id, index: 0}])));
//           }
//         ),
//       ),
//     ]
//   | _ => []
//   };
// };

module Location = {
  type location;
  [@bs.val] external location: location = "location";
  [@bs.set] external setHash: (location, string) => unit = "hash";
  [@bs.get] external hash: location => string = "hash";
};

let force = x =>
  switch (x) {
  | None => failwith("unwrapped a None")
  | Some(x) => x
  };

let permalink = (scene: scene) => {
  Js.log(scene);
  Location.setHash(
    Location.location,
    Serialize.serializeAnyToJson(scene)->Js.Json.stringify,
  );
};

let getInitial = default => {
  let current =
    Location.hash(Location.location)->Js.Global.decodeURIComponent;
  if (String.length(current) > 1) {
    let raw = Js.String2.sliceToEnd(current, ~from=1);
    if (raw.[0] == '{') {
      let data =
        Serialize.unserializeAnyFromJsonUnsafe(raw->Js.Json.parseExn);
      // Hacky data migration!
      (None, Versions.upgrade(data)) |> Js.Promise.resolve;
    } else {
      Gallery.loadState(raw)
      |> Js.Promise.then_(data =>
           (
             switch (data) {
             | None => (None, default)
             | Some(data) => (Some(raw), Versions.upgrade(data))
             }
           )
           |> Js.Promise.resolve
         );
    };
  } else {
    (None, default) |> Js.Promise.resolve;
  };
};

[@react.component]
let make =
    (
      ~selection: selection,
      ~setSelection: selection => unit,
      ~scene,
      ~setScene,
      ~onUndo,
      ~togglePoints,
      ~toggleTraces,
      ~setColor,
    ) => {
  let buttons = [
    ("Permalink", () => permalink(scene)),
    (
      "Clear scene",
      () => {
        Location.setHash(Location.location, "");
        setScene(
          {
            let scene = Api.init();
            let (scene, center) = scene->Api.Point.abs(0., 0.);
            let (scene, _) =
              scene->Api.Point.abs(
                ~sym=Some({center: Api.Ref.id(center), count: 12}),
                0.,
                -200.,
              );
            scene;
          },
        );
      },
    ),
    ("Toggle points", () => togglePoints()),
    ("Toggle traces", () => toggleTraces()),
    ("Undo", onUndo),
    ...(
         switch (selection) {
         | {tiles: [], shapes: [], points: []} => []
         | _ => [
             (
               "Clear",
               (() => setSelection({tiles: [], shapes: [], points: []})),
             ),
           ]
         }
       )
       @ (
         switch (selection) {
         | {shapes: [r], points: [], tiles: []} => [
             (
               "Remove shape",
               (
                 () => {
                   setSelection(Types.emptySelection);
                   setScene({
                     ...scene,
                     shapes: scene.shapes->Belt.Map.String.remove(r.id),
                   });
                 }
               ),
             ),
           ]
         | {shapes: [], points: [], tiles: [r]} => [
             (
               "Remove tile",
               (
                 () => {
                   setSelection(Types.emptySelection);
                   setScene({
                     ...scene,
                     tiles: scene.tiles->Belt.Map.String.remove(r.id),
                   });
                 }
               ),
             ),
           ]
         | _ => []
         }
       ),
  ];
  <div>
    {buttons
     ->Array.of_list
     ->Belt.Array.map(((title, action)) =>
         <button key=title onClick={_ => action()}>
           {React.string(title)}
         </button>
       )
     ->React.array}
    <TranslateEverything scene setScene />
  </div>;
};