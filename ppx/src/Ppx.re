open Ppxlib;

let generate_impl = (~ctxt, (_rec_flag, type_declarations), name) => {

  let loc = Expansion_context.Deriver.derived_item_loc(ctxt);

  let type_declaration = type_declarations |> List.hd;

  let name_string =
    switch (name) {
    | Some(name) => name
    | _ => ""
    };

  let type_name =
    switch (type_declaration) {
    | {ptype_kind: Ptype_variant(_) | Ptype_open, _} =>
      Location.raise_errorf(
        ~loc,
        "Cannot derive accessors for non record or abstract types",
      )
    | {ptype_name, _} => ptype_name.txt
    };

  let generated_data: RecTypes.detectableTypes =
    GenerateData.generate_data(loc, type_declaration);

  switch (type_declaration) {
  | {ptype_kind: Ptype_abstract, _} =>
    StructureData.create_abstract_structure_item(
      loc,
      generated_data,
      type_name,
    )
  | {ptype_kind: Ptype_record(_), _} =>
    StructureData.create_structure_item_list(
      loc,
      generated_data,
      type_name,
      name_string,
    )
  | _ => Location.raise_errorf(~loc, "Only abstract and record types")
  };
};

let args = () => Deriving.Args.(empty +> arg("name", estring(__)));

let impl_generator = Deriving.Generator.V2.make(args(), generate_impl);

let my_deriver = Deriving.add("gql", ~str_type_decl=impl_generator);
