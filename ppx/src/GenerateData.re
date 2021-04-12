open RecTypes;
open Ppxlib;

let rec convert_nested_core_types = (core_types, type_name, loc): detectableTypes => {
  let core_type_string = 
    if (core_types
        |> List.length == 1
        && Utils.isModuleType(core_types |> List.hd, loc)) {
          "module"
  } else if (core_types |> List.length == 1) {
        Utils.extract_string_from_core_type(core_types |> List.hd, loc);
  } else {
    ""
  };

    switch (type_name) {
    | "int" => Integer
    | "string" => String
    | "bool" => Bool
    | "option" =>
      core_type_string != ""
        ? Option(
            convert_nested_core_types(
              (List.mem(core_type_string, Constants.basic_types) || core_type_string == "module")
                ? core_types : Utils.get_core_type(core_types |> List.hd, loc),
              core_type_string,
              loc,
            ),
          )
        : Location.raise_errorf(~loc, "error with type structure")
    | "list"
    | "array" =>
      Array(
        convert_nested_core_types(
          (List.mem(core_type_string, Constants.basic_types) || core_type_string == "module")
            ? core_types : Utils.get_core_type(core_types |> List.hd, loc),
          core_type_string,
          loc,
        ),
      )
    | "module" => {
      let module_name = Utils.get_module(core_types |> List.hd, loc)[0];
      let type_name = Utils.get_module(core_types |> List.hd, loc)[1];
      Module(module_name, type_name);
    }
    | _ => Alias(type_name)
    };
  };

let destruct_label_declaration = (loc, ld): recordItem => {
  let label_name = ld.pld_name.txt;
  let recordItem_type =
    switch (ld.pld_type.ptyp_desc) {
    | Ptyp_constr({txt: Lident(type_name), _}, core_types) =>
      convert_nested_core_types(core_types, type_name, loc)
    | Ptyp_constr({txt: Ldot(Lident(module_name), type_name), _}, _) =>
      Module(module_name, type_name)
    | _ => Location.raise_errorf(~loc, "Could not access ptyp_desc")
    };

  {propertyName: label_name, type_: recordItem_type};
};

let record_accessor_impl =
    (lds: list(label_declaration), loc): detectableTypes => {
  Record(lds |> List.map(destruct_label_declaration(loc)));
};

let generate_data = (loc, type_declarations) => {
  switch (type_declarations) {
  | {ptype_kind: Ptype_variant(_) | Ptype_open, _} =>
    Location.raise_errorf(
      ~loc,
      "Cannot derive accessors for non record or abstract types",
    )
  | {ptype_kind: Ptype_abstract, ptype_manifest, ptype_loc, _} =>
    switch (ptype_manifest) {
    | Some(core_type) =>
      convert_nested_core_types(
        Utils.get_core_type(core_type, loc),
        Utils.extract_string_from_core_type(core_type, loc),
        ptype_loc,
      )
    | _ => Location.raise_errorf(~loc, "No type found")
    }

  | {ptype_kind: Ptype_record(fields), ptype_loc, _} =>
    record_accessor_impl(fields, ptype_loc)
  };
};