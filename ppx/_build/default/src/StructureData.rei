open Ppxlib;
open RecTypes;

let create_structure_item_list:
  (location, detectableTypes, label, label) => list(structure_item);
let create_abstract_structure_item:
  (location, detectableTypes, label) => list(structure_item);