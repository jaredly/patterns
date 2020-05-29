open Types;
module S = Belt.Map.String;

module Shapes = {
  [@react.component]
  let make = (~scene, ~selection, ~setSelection, ~setHovered, ~setScene) => {
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
                 if (isSelected) {
                   setSelection(None);
                 } else {
                   setSelection(Some(Shapes([{id: k, index: 0}])));
                 }
               }
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
let make = (~scene, ~selection, ~setScene, ~setSelection, ~setHovered) => {
  <div>
    <div
      className=Css.(style([fontSize(`percent(130.)), padding(px(8))]))>
      {React.string("Shapes")}
    </div>
    <Shapes scene selection setScene setSelection setHovered />
  </div>;
};