open Ppxlib;
open Ast_builder.Default;


let recordContainsOnlyBasicTypes = (lds, loc): bool => {
  lds
  |> List.fold_left(
       (acc, ld) => acc && Utils.isBasicTypeRec(ld.pld_type.ptyp_desc, loc),
       true,
     );
};

let record_accessor_impl =
    (lds: list(label_declaration), type_name: string, payload, loc): list(structure_item) => {

    let record_struct_item = recordContainsOnlyBasicTypes(lds, loc) ? [Ppx_basicTypes.structure_item_with_basic_types(lds, type_name, payload, loc)] : Ppx_aliasTypes.structure_item_with_alias_types(lds, type_name, payload, loc);
    
    let record_name_struct_item = [[%stri let [%p pvar(~loc, "record_types")] = [%e evar(~loc, "record_types")] @ [[%e estring(~loc, type_name |> String.capitalize_ascii)]]]];

    record_struct_item |> List.append(record_name_struct_item); 
}


let generate_impl = (~ctxt, (_rec_flag, type_declarations), payload) => {
  let loc = Expansion_context.Deriver.derived_item_loc(ctxt);
  let payload_string = 
    switch (payload){
      | Some(payload) => payload
      | _ => ""
}
  ListLabels.map(type_declarations, ~f=(td: type_declaration) =>
    switch (td) {
    | {ptype_kind: Ptype_variant(_) | Ptype_open, _} =>
      Location.raise_errorf(
        ~loc,
        "Cannot derive accessors for non record types",
      )
    | {ptype_kind: Ptype_abstract, ptype_manifest, ptype_name, ptype_loc, _} =>
      switch (ptype_manifest) {
      | Some(core_type) =>
        Ppx_aliasTypes.abstract_accessor_impl(core_type, ptype_name.txt, ptype_loc)
      | _ => Location.raise_errorf(~loc, "No type found")
      }

    | {ptype_kind: Ptype_record(fields), ptype_name, ptype_loc, _} =>
        record_accessor_impl(fields, ptype_name.txt, payload_string, ptype_loc)
    
    }
  ) |> List.concat;
};

let args = () =>
  Deriving.Args.(
    empty +> arg("name", estring(__))
  );

let impl_generator = Deriving.Generator.V2.make(args(), generate_impl);

let my_deriver = Deriving.add("gql", ~str_type_decl=impl_generator);

