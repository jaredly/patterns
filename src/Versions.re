open Types;

let upgrade = data => {
  ...
    Obj.magic(data.presentation) == None
      ? Calculate.translateEverything(data, {x: (-250.), y: (-250.)}) : data,
  tiles: Obj.magic(data.tiles) == None ? Api.empty : data.tiles,
  presentation:
    Obj.magic(data.presentation) == None
      ? Types.defaultPresentation : data.presentation,
};