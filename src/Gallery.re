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

[@bs.val] [@bs.scope "URL"]
external revokeObjectURL: string => unit = "revokeObjectURL";

let useBlob = blob => {
  let url = React.useMemo1(() => createObjectURL(blob), [|blob|]);
  React.useEffect1(() => {Some(() => revokeObjectURL(url))}, [|url|]);
  url;
};

module BlobImage = {
  [@react.component]
  let make = (~blob, ~children) => {
    let url = useBlob(blob);
    children(url);
  };
};

module Inner = {
  [@react.component]
  let make = (~current, ~onLoad, ~onSave, ~initial) => {
    let (screenshots, setScreenshots) = Hooks.useState(initial);
    <div>
      <button
        onClick={_ => {
          let (id, blob) = onSave();
          setScreenshots(
            screenshots->Belt.Array.map(item =>
              fst(item) == id ? (id, Some(blob)) : item
            ),
          );
        }}>
        {React.string("Save")}
      </button>
      {screenshots == [||] ? React.string("No saved screenshots") : React.null}
      {screenshots
       ->Belt.Array.map(((id, blob)) => {
           switch (blob) {
           | None => React.null
           | Some(blob) =>
             //  let url = createObjectURL(blob);
             <BlobImage blob key=id>
               {(
                  src =>
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
                      src
                    />
                )}
             </BlobImage>
           }
         })
       ->React.array}
    </div>;
  };
};

[@react.component]
let make = (~current, ~onLoad, ~onSave) => {
  let screenshots = Hooks.usePromise(getScreenshots);
  switch (screenshots) {
  | None => <div> {React.string("loading...")} </div>
  | Some(screenshots) => <Inner current onLoad onSave initial=screenshots />
  };
};