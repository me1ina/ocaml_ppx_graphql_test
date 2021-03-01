/*open Ppxlib;

  let generate_impl:
    (
      ~ctxt: Expansion_context.Deriver.t,
      ('a, list(type_declaration)),
      option(label)
    ) =>
    list(structure_item); */

let my_deriver: Ppxlib.Deriving.t;