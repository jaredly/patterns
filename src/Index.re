// Entry point

[@bs.val] external document: Js.t({..}) = "document";

let hash = Controls.Location.hash();

if (hash->Js.String2.startsWith("#test")) {
  ReactDOMRe.render(<Tests which=hash />, document##getElementById("root"));
} else {
  App.loadInitial()
  |> Js.Promise.then_(initial => {
       ReactDOMRe.render(<App initial />, document##getElementById("root"));
       Js.Promise.resolve();
     })
  |> ignore;
};