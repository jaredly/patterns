let useState = initial => React.useReducer((_, a) => a, initial);

let usePromise = fn => {
  let (value, setValue) = useState(None);
  React.useEffect1(
    () => {
      fn()
      |> Js.Promise.then_(value => {
           setValue(Some(value));
           Js.Promise.resolve();
         })
      |> ignore;
      None;
    },
    [|fn|],
  );
  value;
};