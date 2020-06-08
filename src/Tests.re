// Hello
open Types;
open Calculate;

type bounds = {
  tl: pos,
  br: pos,
};

let bounds = shape =>
  switch (shape) {
  | CLine({p1, p2}) => {
      tl: {
        x: min(p1.x, p2.x),
        y: min(p1.y, p2.y),
      },
      br: {
        x: max(p1.x, p2.x),
        y: max(p1.y, p2.y),
      },
    }
  | CCircle({center, r}) => {
      tl: {
        x: center.x -. r,
        y: center.y -. r,
      },
      br: {
        x: center.x +. r,
        y: center.y +. r,
      },
    }
  | CCirclePart({center, r}) => {
      tl: {
        x: center.x -. r,
        y: center.y -. r,
      },
      br: {
        x: center.x +. r,
        y: center.y +. r,
      },
    }
  };

let addBounds = ({tl: a0, br: a1}, {tl: b0, br: b1}) => {
  tl: {
    x: min(a0.x, b0.x),
    y: min(a0.y, b0.y),
  },
  br: {
    x: max(a1.x, b1.x),
    y: max(a1.y, b1.y),
  },
};

let constrain = (shapes, desired: bounds) => {
  let fullBounds =
    shapes->Belt.List.reduce(None, (current, shape) =>
      switch (current) {
      | Some(m) => Some(addBounds(m, bounds(shape)))
      | None => Some(bounds(shape))
      }
    );
  switch (fullBounds) {
  | None => shapes
  | Some(full) =>
    let w = full.br.x -. full.tl.x;
    let dw = desired.br.x -. desired.tl.x;
    let h = full.br.y -. full.tl.y;
    let dh = desired.br.y -. desired.tl.y;
    let scale = min(dw /. w, dh /. h);
    let dx = desired.tl.x -. full.tl.x *. scale;
    let dy = desired.tl.y -. full.tl.y *. scale;
    // ({x: dx, y: dy}, scale)
    shapes->Belt.List.map(shape =>
      Calculate.transformConcrete(shape, {x: dx, y: dy}, scale)
    );
  };
};

let cases = [
  [
    CCirclePart({
      center: {
        x: (-0.000000),
        y: (-200.000000),
      },
      r: 286.985504,
      theta0: 1.143161,
      theta1: 1.364523,
      clockwise: true,
    }),
    CCirclePart({
      center: {
        x: (-190.211303),
        y: (-61.803399),
      },
      r: 286.985504,
      theta0: 0.520432,
      theta1: 0.741795,
      clockwise: true,
    }),
    CCirclePart({
      center: {
        x: 263.156875,
        y: (-85.504852),
      },
      r: 325.279786,
      theta0: 1.884956,
      theta1: 2.408812,
      clockwise: true,
    }),
    CCirclePart({
      center: {
        x: (-162.639893),
        y: 223.854608,
      },
      r: 325.279786,
      theta0: (-0.523856),
      theta1: (-0.000000),
      clockwise: true,
    }),
  ],
];
// ->Belt.List.map(shapes =>
//     constrain(shapes, {
//                         tl: {
//                           x: 0.,
//                           y: 0.,
//                         },
//                         br: {
//                           x: 200.,
//                           y: 200.,
//                         },
//                       })
//   );

// let fiveCircles = {
//   let r = 60.;
//   let pc = {x: 100., y: 140.};
//   let p0 = {x: 100., y: 100.};
//   let p1 = rotateAround(p0, pc, tau *. 1. /. 5.);
//   let p2 = rotateAround(p0, pc, tau *. 2. /. 5.);
//   let p3 = rotateAround(p0, pc, tau *. 3. /. 5.);
//   let p4 = rotateAround(p0, pc, tau *. 4. /. 5.);

//   switch (
//     intersectCircles(p0, r, p2, r),
//     intersectCircles(p1, r, p3, r),
//     intersectCircles(p2, r, p4, r),
//     intersectCircles(p3, r, p0, r),
//     intersectCircles(p4, r, p1, r),
//   ) {
//   | (
//       [p01a, p01b],
//       [p12a, p12b],
//       [p23a, p23b],
//       [p34a, p34b],
//       [p40a, p40b],
//     ) => (
//       [
//         [] // CCirclePart({
//         //   center: p0,
//         //   r,
//         //   theta0: angleTo(dpos(p0, ))
//         // }),
//       ],
//       [
//         (p0, "p0"),
//         (p1, "p1"),
//         (p2, "p2"),
//         (p3, "p3"),
//         (p4, "p4"),
//         (p01a, "p01a"),
//         (p01b, "p01b"),
//         (p12a, "p12a"),
//         (p12b, "p12b"),
//         (p23a, "p23a"),
//         (p34a, "p34a"),
//         (p40a, "p40a"),
//       ],
//     )
//   | _ => ([], [])
//   };
// };

let threeCircles = {
  let r = 60.;
  let p0 = {x: 100., y: 100.};
  let pc = {x: 100., y: 150.};
  let p1 = rotateAround(p0, pc, tau /. 3.);
  let p2 = rotateAround(p0, pc, tau *. 2. /. 3.);
  switch (
    intersectCircles(p0, r, p1, r),
    intersectCircles(p0, r, p2, r),
    intersectCircles(p1, r, p2, r),
  ) {
  | ([p01a, p01b], [p02a, p02b], [p12a, p12b]) => (
      [
        [
          CLine({p2: p12b, p1: p02a}),
          CCirclePart({
            center: p0,
            r,
            theta0: angleTo(dpos(p0, p01b)),
            theta1: angleTo(dpos(p0, p02b)),
            clockwise: true,
          }),
          CCirclePart({
            center: p2,
            r,
            theta0: angleTo(dpos(p2, p02b)),
            theta1: angleTo(dpos(p2, p12b)),
            clockwise: true,
          }),
          CCirclePart({
            center: p1,
            r,
            theta0: angleTo(dpos(p1, p12a)),
            theta1: angleTo(dpos(p1, p01b)),
            clockwise: true,
          }),
          CCirclePart({
            center: p2,
            r,
            theta1: angleTo(dpos(p2, p02a)),
            theta0: angleTo(dpos(p2, p12a)),
            clockwise: false,
          }),
        ],
        [
          CCirclePart({
            center: p0,
            r,
            theta0: angleTo(dpos(p0, p01b)),
            theta1: angleTo(dpos(p0, p02b)),
            clockwise: true,
          }),
          CCirclePart({
            center: p2,
            r,
            theta0: angleTo(dpos(p2, p02b)),
            theta1: angleTo(dpos(p2, p12b)),
            clockwise: true,
          }),
          CCirclePart({
            center: p1,
            r,
            theta0: angleTo(dpos(p1, p12b)),
            theta1: angleTo(dpos(p1, p01a)),
            clockwise: true,
          }),
          CCirclePart({
            center: p0,
            r,
            theta0: angleTo(dpos(p0, p01a)),
            theta1: angleTo(dpos(p0, p02a)),
            clockwise: true,
          }),
          CCirclePart({
            center: p2,
            r,
            theta0: angleTo(dpos(p2, p02a)),
            theta1: angleTo(dpos(p2, p12a)),
            clockwise: true,
          }),
          CCirclePart({
            center: p1,
            r,
            theta0: angleTo(dpos(p1, p12a)),
            theta1: angleTo(dpos(p1, p01b)),
            clockwise: true,
          }),
        ],
      ],
      // [p01b, p02b, p12a],
      [] // (p0, "p0"),
      // (p1, "p1"),
      // (p2, "p2"),
      // (p01b, "p01b"),
      // (p02b, "p02b"),
      // (p12a, "p12a"),
      // (p12b, "p12b"),
      // (p01a, "p01a"),
      // (p02a, "p02a"),
    )
  | _ => ([], [])
  };
};

let twoCircles = (c1, r1, c2, r2) => {
  let crosses = Calculate.intersectCircles(c1, r1, c2, r2);
  switch (crosses) {
  | [p1, p2] => [
      [
        CCirclePart({
          center: c1,
          r: r1,
          theta0: angleTo(dpos(c1, p1)),
          theta1: angleTo(dpos(c1, p2)),
          clockwise: true,
        }),
        CCirclePart({
          center: c2,
          r: r2,
          theta0: angleTo(dpos(c2, p1)),
          theta1: angleTo(dpos(c2, p2)),
          clockwise: true,
        }),
      ],
      //
      [
        CCirclePart({
          center: c1,
          r: r1,
          theta0: angleTo(dpos(c1, p1)),
          theta1: angleTo(dpos(c1, p2)),
          clockwise: false,
        }),
        CCirclePart({
          center: c2,
          r: r2,
          theta0: angleTo(dpos(c2, p1)),
          theta1: angleTo(dpos(c2, p2)),
          clockwise: true,
        }),
      ],
      //
      [
        CCirclePart({
          center: c1,
          r: r1,
          theta0: angleTo(dpos(c1, p1)),
          theta1: angleTo(dpos(c1, p2)),
          clockwise: false,
        }),
        CCirclePart({
          center: c2,
          r: r2,
          theta0: angleTo(dpos(c2, p1)),
          theta1: angleTo(dpos(c2, p2)),
          clockwise: false,
        }),
      ],
      //
      [
        CCirclePart({
          center: c1,
          r: r1,
          theta0: angleTo(dpos(c1, p1)),
          theta1: angleTo(dpos(c1, p2)),
          clockwise: true,
        }),
        CCirclePart({
          center: c2,
          r: r2,
          theta0: angleTo(dpos(c2, p1)),
          theta1: angleTo(dpos(c2, p2)),
          clockwise: false,
        }),
      ],
      [
        CCirclePart({
          center: c1,
          r: r1,
          theta0: angleTo(dpos(c1, p1)),
          theta1: angleTo(dpos(c1, p2)),
          clockwise: true,
        }),
        CCirclePart({
          center: c2,
          r: r2,
          theta1: angleTo(dpos(c2, p1)),
          theta0: angleTo(dpos(c2, p2)),
          clockwise: false,
        }),
      ],
      [
        CCirclePart({
          center: c1,
          r: r1,
          theta0: angleTo(dpos(c1, p1)),
          theta1: angleTo(dpos(c1, p2)),
          clockwise: false,
        }),
        CCirclePart({
          center: c2,
          r: r2,
          theta1: angleTo(dpos(c2, p1)),
          theta0: angleTo(dpos(c2, p2)),
          clockwise: false,
        }),
      ],
      [
        CCirclePart({
          center: c1,
          r: r1,
          theta0: angleTo(dpos(c1, p1)),
          theta1: angleTo(dpos(c1, p2)),
          clockwise: false,
        }),
        CCirclePart({
          center: c2,
          r: r2,
          theta1: angleTo(dpos(c2, p1)),
          theta0: angleTo(dpos(c2, p2)),
          clockwise: true,
        }),
      ],
      [
        CCirclePart({
          center: c1,
          r: r1,
          theta0: angleTo(dpos(c1, p1)),
          theta1: angleTo(dpos(c1, p2)),
          clockwise: true,
        }),
        CCirclePart({
          center: c2,
          r: r2,
          theta0: angleTo(dpos(c2, p2)),
          theta1: angleTo(dpos(c2, p1)),
          clockwise: true,
        }),
      ],
    ]
  | _ => assert(false)
  };
};

[@react.component]
let make = (~which) => {
  let dots = []; // fiveCircles |> snd;
  let tests =
    [|
      cases,
      // fiveCircles |> fst,
      threeCircles |> fst,
      twoCircles({x: 50., y: 50.}, 40., {x: 100., y: 100.}, 65.),
      twoCircles({x: 80., y: 80.}, 60., {x: 110., y: 110.}, 75.),
      twoCircles({x: 50., y: 50.}, 50., {x: 100., y: 100.}, 50.),
      {
        let p0 = {x: 30., y: 30.};
        let p4 = {x: 110., y: 110.};
        let center = {x: 100., y: 100.};
        let r = 50.;
        switch (Calculate.pointCircle(center, r, p0)) {
        | None => []
        | Some((p1, p2)) => [
            [
              CLine({p1: p0, p2: p1}),
              CLine({p1: p0, p2}),
              CCirclePart({
                center,
                r,
                theta0: angleTo(dpos(center, p1)),
                theta1: angleTo(dpos(center, p2)),
                clockwise: true,
              }),
            ],
            [
              CLine({p1: p0, p2: p1}),
              CLine({p1: p0, p2}),
              CCirclePart({
                center,
                r,
                theta0: angleTo(dpos(center, p1)),
                theta1: angleTo(dpos(center, p2)),
                clockwise: false,
              }),
            ],
            [
              CLine({p1: p4, p2: p1}),
              CLine({p1: p4, p2}),
              CCirclePart({
                center,
                r,
                theta0: angleTo(dpos(center, p1)),
                theta1: angleTo(dpos(center, p2)),
                clockwise: true,
              }),
            ],
            [
              CLine({p1: p4, p2: p1}),
              CLine({p1: p4, p2}),
              CCirclePart({
                center,
                r,
                theta0: angleTo(dpos(center, p1)),
                theta1: angleTo(dpos(center, p2)),
                clockwise: false,
              }),
            ],
          ]
        };
      },
      // {
      //   [
      //     [
      //       CCirclePart({
      //         center: {
      //           x: 100.,
      //           y: 100.,
      //         },
      //         r: 50.,
      //         theta0: 0.,
      //         theta1: pi2,
      //         clockwise: true,
      //       }),
      //       CCirclePart({
      //         center: {
      //           x: 125.,
      //           y: 100.,
      //         },
      //         r: 25.,
      //         theta0: 0.,
      //         theta1: pi2,
      //         clockwise: true,
      //       }),
      //       CLine({
      //         p1: {
      //           x: 125.,
      //           y: 50.,
      //         },
      //         p2: {
      //           x: 100.,
      //           y: 50.,
      //         },
      //       }),
      //     ],
      //   ];
      // },
    |]
    ->Belt.List.concatMany
    ->Array.of_list;

  let rotations = (orig, count) => {
    let by = tau /. float_of_int(count);
    let rec loop = i =>
      if (i == 0) {
        [];
      } else {
        [
          orig->Belt.List.map(shape =>
            rotateShape(shape, {x: 100., y: 100.}, by *. float_of_int(i))
          ),
          ...loop(i - 1),
        ];
      };
    loop(count);
  };

  let tests =
    switch (Js.String.split("/", which)) {
    | [|_, one|] =>
      rotations(tests[int_of_string(one)], 12)->Belt.List.toArray
    | _ => tests
    };

  let wrap = 4;
  let w = 200.;
  let h = 200.;

  <div>
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width={string_of_int(1000) ++ "px"}
      height={string_of_int(1000) ++ "px"}>
      {tests
       ->Belt.Array.mapWithIndex((i, sides) => {
           let center = {
             x: -. float_of_int(i mod wrap) *. w,
             y: -. float_of_int(i / wrap) *. h,
           };

           <React.Fragment key={string_of_int(i)}>
             <path
               fill="red"
               d={Canvas.polyPath({center, zoom: 1.}, sides, 0., false)}
               stroke="black"
               strokeWidth="3"
               className=Css.(
                 style([
                   cursor(`pointer),
                   opacity(0.5),
                   //  hover([
                   //  ])
                 ])
               )
             />
             <path
               fill="blue"
               d={Canvas.polyPath({center, zoom: 1.}, sides, 5., false)}
               stroke="black"
               strokeWidth="3"
               className=Css.(
                 style([
                   cursor(`pointer),
                   opacity(0.5),
                   //  hover([
                   //  ])
                 ])
               )
             />
             {sides
              ->Belt.List.toArray
              ->Belt.Array.map(shape =>
                  [
                    <path
                      stroke="red"
                      fill="none"
                      strokeWidth="1"
                      d={Canvas.pathForShape({center, zoom: 1.}, shape)}
                    />,
                    ...Canvas.shapeDebugPoints({center, zoom: 1.}, shape),
                  ]
                )
              ->Belt.List.concatMany
              ->Belt.List.toArray
              ->React.array}
           </React.Fragment>;
           //  <path
           //    fill="transparent"
           //    d={Canvas.polyPath({center, zoom: 1.}, sides, 5., true)}
           //    stroke="green"
           //    strokeWidth="10"
           //    className=Css.(
           //      style([
           //        cursor(`pointer),
           //        opacity(0.5),
           //        //  hover([
           //        //  ])
           //      ])
           //    )
           //  />
         })
       ->React.array}
      {dots
       ->Belt.List.toArray
       ->Belt.Array.mapWithIndex((i, (pos, name)) => {
           <React.Fragment key={string_of_int(i)}>
             <circle
               cx={pos.x |> Js.Float.toString}
               cy={pos.y |> Js.Float.toString}
               r="5"
               fill="red"
             />
             <text
               x={pos.x +. 5. |> Js.Float.toString}
               y={pos.y -. 10. |> Js.Float.toString}>
               {React.string(name)}
             </text>
           </React.Fragment>
         })
       ->React.array}
    </svg>
  </div>;
};