// Hello
open Types;
open Calculate;

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
  let tests =
    [|
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

  let tests =
    switch (Js.String.split("/", which)) {
    | [|_, one|] => [|tests[int_of_string(one)]|]
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
             <path
               fill="transparent"
               d={Canvas.polyPath({center, zoom: 1.}, sides, 5., true)}
               stroke="green"
               strokeWidth="10"
               className=Css.(
                 style([
                   cursor(`pointer),
                   opacity(0.5),
                   //  hover([
                   //  ])
                 ])
               )
             />
           </React.Fragment>;
         })
       ->React.array}
    </svg>
  </div>;
};