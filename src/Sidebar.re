open Types;
module S = Belt.Map.String;

[@react.component]
let make = (~scene, ~selection, ~setScene, ~setSelection, ~setHovered) => {
  <div>
    {scene.shapes
     ->S.toArray
     ->Belt.Array.map(((k, shape)) => {
         <div
           key=k
           onMouseOver={_ => setHovered(Some(`Shape({id: k, index: 0})))}
           onMouseOut={_ => setHovered(None)}
           onClick={_ => setSelection(Some(Shapes([{id: k, index: 0}])))}>
           {React.string(k)}
           {switch (selection) {
            | Some(Shapes(s)) when s->Belt.List.some(s => s.id == k) =>
              <div className=Css.(style([padding(px(16))]))>
                {React.string("ok folks")}
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
            | _ => React.null
            }}
         </div>
       })
     ->React.array}
  </div>;
};