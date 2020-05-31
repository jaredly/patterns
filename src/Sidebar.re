open Types;
module S = Belt.Map.String;

module ColorPicker = {
  let rawColors = "fbb4aeb3cde3ccebc5decbe4fed9a6ffffcce5d8bdfddaecf2f2f2";
  let rec loop = raw =>
    if (raw == "") {
      [];
    } else {
      let chunk = Js.String.slice(~from=0, ~to_=6, raw);
      let rest = Js.String.sliceToEnd(~from=6, raw);
      ["#" ++ chunk, ...loop(rest)];
    };
  let colors = loop(rawColors)->Belt.List.toArray;

  [@react.component]
  let make = (~onPick) => {
    let (open_, setOpen) = React.useState(() => false);

    <div>
      <button onClick={evt => setOpen(open_ => !open_)}>
        {React.string("Color picker")}
      </button>
      {open_
         ? <div>
             {colors
              ->Belt.Array.map(hex =>
                  <button
                    onClick={_ => {
                      onPick(hex);
                      setOpen(_ => false);
                    }}
                    style={ReactDOMRe.Style.make(~backgroundColor=hex, ())}
                    className=Css.(
                      style([
                        width(px(20)),
                        height(px(20)),
                        borderStyle(`none),
                        cursor(`pointer),
                      ])
                    )
                  />
                )
              ->React.array}
           </div>
         : React.null}
    </div>;
  };
};

module Tiles = {
  [@react.component]
  let make = (~scene, ~selection, ~setSelection, ~setHovered, ~setScene) => {
    <React.Fragment>
      {scene.tiles
       ->S.toArray
       ->Belt.Array.map(((k, tile)) => {
           <div>
             <div> {React.string("Tile")} </div>
             <div>
               <div
                 style={ReactDOMRe.Style.make(
                   ~backgroundColor=tile.color,
                   (),
                 )}
                 className=Css.(
                   style([
                     width(px(20)),
                     height(px(20)),
                     display(`inlineBlock),
                   ])
                 )
               />
               <input
                 value={tile.color}
                 onChange={evt => {
                   let color = evt->ReactEvent.Form.target##value;
                   setScene({
                     ...scene,
                     tiles:
                       scene.tiles->Belt.Map.String.set(k, {...tile, color}),
                   });
                 }}
               />
               <ColorPicker
                 onPick={color =>
                   setScene({
                     ...scene,
                     tiles:
                       scene.tiles->Belt.Map.String.set(k, {...tile, color}),
                   })
                 }
               />
             </div>
             <div>
               {React.string("Margin: ")}
               <input
                 value={tile.margin->Js.Float.toString}
                 onChange={evt => {
                   let margin =
                     evt->ReactEvent.Form.target##value->Js.Float.fromString;
                   setScene({
                     ...scene,
                     tiles:
                       scene.tiles
                       ->Belt.Map.String.set(k, {...tile, margin}),
                   });
                 }}
               />
             </div>
             <div>
               {React.string("Order: ")}
               <input
                 value={tile.order->Js.Float.toString}
                 onChange={evt => {
                   let order =
                     evt->ReactEvent.Form.target##value->Js.Float.fromString;
                   setScene({
                     ...scene,
                     tiles:
                       scene.tiles->Belt.Map.String.set(k, {...tile, order}),
                   });
                 }}
               />
             </div>
             <div>
               {React.string("Symmetry: ")}
               {switch (tile.sym) {
                | None => React.null
                | Some(sym) =>
                  <input
                    value={string_of_int(sym.count)}
                    onChange={evt => {
                      let value = evt->ReactEvent.Form.target##value;
                      let count = int_of_string(value);
                      setScene({
                        ...scene,
                        tiles:
                          scene.tiles
                          ->Belt.Map.String.set(
                              k,
                              {...tile, sym: Some({...sym, count})},
                            ),
                      });
                    }}
                  />
                }}
             </div>
           </div>
         })
       ->React.array}
    </React.Fragment>;
  };
};

module Points = {
  [@react.component]
  let make = (~scene, ~setScene) => {
    <div>
      {scene.points
       ->S.toArray
       ->Belt.Array.map(((k, point)) => {
           <div key=k>
             {React.string("Point")}
             <div>
               {React.string("Symmetry: ")}
               {switch (point.sym) {
                | None => React.null
                | Some(sym) =>
                  <input
                    value={string_of_int(sym.count)}
                    onChange={evt => {
                      let value = evt->ReactEvent.Form.target##value;
                      let count = int_of_string(value);
                      setScene({
                        ...scene,
                        points:
                          scene.points
                          ->Belt.Map.String.set(
                              k,
                              {...point, sym: Some({...sym, count})},
                            ),
                      });
                    }}
                  />
                }}
             </div>
           </div>
         })
       ->React.array}
    </div>;
  };
};

module Shapes = {
  [@react.component]
  let make =
      (
        ~scene,
        ~selection,
        ~selectShape,
        ~setSelection,
        ~setHovered,
        ~setScene,
      ) => {
    <React.Fragment>
      {scene.shapes
       ->S.toArray
       ->Belt.Array.map(((k, shape)) => {
           let isSelected =
             switch (selection) {
             | Some(Shapes(s)) when s->Belt.List.some(s => s.id == k) => true
             | _ => false
             };
           <div
             key=k
             onMouseOver={_ => setHovered(Some(`Shape({id: k, index: 0})))}
             onMouseOut={_ => setHovered(None)}>
             <div
               onClick={_ =>
                 //  if (isSelected) {
                 //    setSelection(None);
                 //  } else {
                 //    setSelection(Some(Shapes([{id: k, index: 0}])));
                 //  }
                 selectShape({id: k, index: 0})}
               style={ReactDOMRe.Style.make(
                 ~borderColor=
                   switch (shape.color) {
                   | None => "transparent"
                   | Some(color) => color
                   },
                 (),
               )}
               className=Css.(
                 style([
                   padding(px(8)),
                   cursor(`pointer),
                   borderColor(`transparent),
                   borderStyle(`solid),
                   margin(px(2)),
                   borderWidth(px(1)),
                   fontWeight(isSelected ? `bold : inherit_),
                   hover([backgroundColor(`hex("eee"))]),
                 ])
               )>
               {React.string(
                  switch (shape.kind) {
                  | Line(_) => "Line"
                  | Circle(_) => "Circle"
                  | CirclePart(_) => "Arc"
                  },
                )}
             </div>
             {isSelected
                ? <div className=Css.(style([padding(px(16))]))>
                    <div>
                      {React.string("Symmetry: ")}
                      {switch (shape.sym) {
                       | None => React.null
                       | Some(sym) =>
                         <input
                           value={string_of_int(sym.count)}
                           onChange={evt => {
                             let value = evt->ReactEvent.Form.target##value;
                             let count = int_of_string(value);
                             setScene({
                               ...scene,
                               shapes:
                                 scene.shapes
                                 ->Belt.Map.String.set(
                                     k,
                                     {...shape, sym: Some({...sym, count})},
                                   ),
                             });
                           }}
                         />
                       }}
                    </div>
                    <div>
                      {React.string("Color: ")}
                      <input
                        value={
                          switch (shape.color) {
                          | None => ""
                          | Some(color) => color
                          }
                        }
                        onChange={evt => {
                          let value = evt->ReactEvent.Form.target##value;
                          setScene({
                            ...scene,
                            shapes:
                              scene.shapes
                              ->Belt.Map.String.set(
                                  k,
                                  {
                                    ...shape,
                                    color:
                                      if (value == "") {
                                        None;
                                      } else {
                                        Some(value);
                                      },
                                  },
                                ),
                          });
                        }}
                      />
                    </div>
                  </div>
                : React.null}
           </div>;
         })
       ->React.array}
    </React.Fragment>;
  };
};

[@react.component]
let make =
    (
      ~scene,
      ~selection,
      ~selectPoint,
      ~selectShape,
      ~setScene,
      ~setSelection,
      ~setHovered,
    ) => {
  <div
    className=Css.(
      style([height(px(1000)), display(`flex), flexDirection(`column)])
    )>
    <div
      className=Css.(style([fontSize(`percent(130.)), padding(px(8))]))>
      {React.string("Tiles")}
    </div>
    <div
      className=Css.(
        style([flex(`num(1.)), minHeight(`px(0)), overflow(`auto)])
      )>
      <Tiles scene selection setScene setSelection setHovered />
    </div>
    <div
      className=Css.(style([fontSize(`percent(130.)), padding(px(8))]))>
      {React.string("Shapes")}
    </div>
    <div
      className=Css.(
        style([flex(`num(1.)), minHeight(`px(0)), overflow(`auto)])
      )>
      <Shapes scene selection selectShape setScene setSelection setHovered />
    </div>
    <div
      className=Css.(style([fontSize(`percent(130.)), padding(px(8))]))>
      {React.string("Points")}
    </div>
    <div
      className=Css.(
        style([flex(`num(1.)), minHeight(`px(0)), overflow(`auto)])
      )>
      <Points scene setScene />
    </div>
  </div>;
};