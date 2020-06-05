open Types;
module S = Belt.Map.String;

module ColorPicker = {
  let rec parseColors = raw =>
    if (raw == "") {
      [];
    } else {
      let chunk = Js.String.slice(~from=0, ~to_=6, raw);
      let rest = Js.String.sliceToEnd(~from=6, raw);
      ["#" ++ chunk, ...parseColors(rest)];
    };
  let sets =
    [
      "e41a1c377eb84daf4a984ea3ff7f00ffff33a65628f781bf999999",
      "66c2a5fc8d628da0cbe78ac3a6d854ffd92fe5c494b3b3b3",
      "8dd3c7ffffb3bebadafb807280b1d3fdb462b3de69fccde5d9d9d9bc80bdccebc5ffed6f",
      "fbb4aeb3cde3ccebc5decbe4fed9a6ffffcce5d8bdfddaecf2f2f2",
      "b3e2cdfdcdaccbd5e8f4cae4e6f5c9fff2aef1e2cccccccc",
      "7fc97fbeaed4fdc086ffff99386cb0f0027fbf5b17666666",
      "1b9e77d95f027570b3e7298a66a61ee6ab02a6761d666666",
    ]
    ->Belt.List.map(parseColors);
  // let colors = parseColors(pastels)->Belt.List.toArray;

  [@react.component]
  let make = (~onPick) => {
    let (open_, setOpen) = React.useState(() => false);

    <div>
      <button onClick={evt => setOpen(open_ => !open_)}>
        {React.string("Color picker")}
      </button>
      {open_
         ? <div>
             {sets
              ->Belt.List.toArray
              ->Belt.Array.map(set => {
                  <div>
                    {set
                     ->Belt.List.toArray
                     ->Belt.Array.map(hex =>
                         <button
                           key=hex
                           onClick={_ => {
                             onPick(hex);
                             setOpen(_ => false);
                           }}
                           style={ReactDOMRe.Style.make(
                             ~backgroundColor=hex,
                             (),
                           )}
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
                })
              ->React.array}
           </div>
         : React.null}
    </div>;
  };
};

module Tiles = {
  [@react.component]
  let make =
      (
        ~scene,
        ~selection: selection,
        ~selectTile,
        ~deselectTile,
        ~setSelection,
        ~setHovered,
        ~setScene,
      ) => {
    <div className=Css.(style([padding(px(2))]))>
      {scene.tiles
       ->S.toArray
       ->Belt.Array.map(((k, tile)) => {
           let isSelected = selection.tiles->Belt.List.some(s => s.id == k);
           <div
             key=k
             className=Css.(
               style(
                 isSelected ? [outline(px(2), `solid, hex("f0a"))] : [],
               )
             )>
             <div
               onClick={_ =>
                 isSelected ? deselectTile(k) : selectTile({id: k, index: 0})
               }
               className=Css.(
                 style([
                   cursor(`pointer),
                   padding(px(4)),
                   display(`flex),
                   alignItems(`center),
                   hover([backgroundColor(hex("eee"))]),
                 ])
               )>
               <div
                 style={ReactDOMRe.Style.make(
                   ~backgroundColor=tile.color,
                   (),
                 )}
                 className=Css.(
                   style([
                     width(px(20)),
                     height(px(20)),
                     marginRight(px(4)),
                     display(`inlineBlock),
                   ])
                 )
               />
               {React.string("Tile")}
             </div>
             {isSelected
                ? <React.Fragment>
                    <div>
                      <input
                        value={tile.color}
                        onChange={evt => {
                          let color = evt->ReactEvent.Form.target##value;
                          setScene({
                            ...scene,
                            tiles:
                              scene.tiles
                              ->Belt.Map.String.set(k, {...tile, color}),
                          });
                        }}
                      />
                      <ColorPicker
                        onPick={color =>
                          setScene({
                            ...scene,
                            tiles:
                              scene.tiles
                              ->Belt.Map.String.set(k, {...tile, color}),
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
                            evt->ReactEvent.Form.target##value
                            ->Js.Float.fromString;
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
                            evt->ReactEvent.Form.target##value
                            ->Js.Float.fromString;
                          setScene({
                            ...scene,
                            tiles:
                              scene.tiles
                              ->Belt.Map.String.set(k, {...tile, order}),
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
                  </React.Fragment>
                : React.null}
           </div>;
         })
       ->React.array}
    </div>;
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
        ~selection: selection,
        ~selectShape,
        ~setSelection,
        ~setHovered,
        ~setScene,
      ) => {
    <React.Fragment>
      {scene.shapes
       ->S.toArray
       ->Belt.Array.map(((k, shape)) => {
           let isSelected = selection.shapes->Belt.List.some(s => s.id == k);
           <div
             key=k
             onMouseOver={_ => setHovered(Some(`Shape({id: k, index: 0})))}
             onMouseOut={_ => setHovered(None)}>
             <div
               onClick={_ => selectShape({id: k, index: 0})}
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
      ~selectTile,
      ~deselectTile,
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
      <Tiles
        scene
        selection
        setScene
        setSelection
        setHovered
        selectTile
        deselectTile
      />
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