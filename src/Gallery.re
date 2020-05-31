/*

 Ok, so we're saving stuff, right?
 Which means that a given thing needs an ID

 So when you go to save it, if there's no ID, it gets an ID.
 And then the URL updates to reflect that ID. And thereafter saving updates it.
 Good news.

 Ok, so I'm serializing the data.
 But then also the svg, right?
 Sure.
 With traces & dots? Maybe.
 I'm guessing the svg would be smaller than the full thing, right?

 */

type blob;

let saveState = (id, state: Types.scene) => {
  let json = Serialize.serializeAnyToJson(state);
  LocalForage.setItem(LocalForage.toKey(id), json);
};

let loadState = id => {
  LocalForage.getItem(LocalForage.toKey(id))
  |> Js.Promise.then_(v =>
       Js.Promise.resolve(
         switch (v) {
         | None => None
         | Some(v) => Serialize.unserializeAnyFromJsonUnsafe(v)
         },
       )
     );
};

let saveScreenshot = (id, blob) =>
  LocalForage.setItem(LocalForage.toKey(id ++ ":img"), blob);
let getScreenshot = (id: string): Js.Promise.t(option(blob)) =>
  LocalForage.getItem(LocalForage.toKey(id ++ ":img"));

let getScreenshots = () => {
  LocalForage.keys()
  |> Js.Promise.then_(keys => {
       keys
       ->Belt.Array.keep(key => key->Js.String2.endsWith(":img"))
       ->Belt.Array.map(key =>
           key->Js.String2.slice(~from=0, ~to_=String.length(key) - 4)
         )
       ->Belt.Array.map(key =>
           getScreenshot(key)
           |> Js.Promise.then_(blob => (key, blob) |> Js.Promise.resolve)
         )
       |> Js.Promise.all
     });
};

[@bs.val] [@bs.scope "URL"]
external createObjectURL: blob => string = "createObjectURL";

[@react.component]
let make = (~current, ~onLoad, ~onSave) => {
  let screenshots = Hooks.usePromise(getScreenshots);
  switch (screenshots) {
  | None => <div> {React.string("loading...")} </div>
  | Some([||]) =>
    <div>
      <button onClick={_ => onSave()}> {React.string("Save")} </button>
      {React.string("No saved screenshots")}
    </div>
  | Some(screenshots) =>
    <div>
      <button onClick={_ => onSave()}> {React.string("Save")} </button>
      {screenshots
       ->Belt.Array.map(((id, blob)) => {
           switch (blob) {
           | None => React.null
           | Some(blob) =>
             let url = createObjectURL(blob);
             <img
               onClick={_ => onLoad(id)}
               className=Css.(
                 style(
                   [width(px(100)), height(px(100))]
                   @ (
                     current == Some(id)
                       ? [outline(px(3), `solid, hex("faa"))] : []
                   ),
                 )
               )
               src=url
             />;
           }
         })
       ->React.array}
    </div>
  };
};