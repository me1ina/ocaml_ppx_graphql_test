open Ppxlib;
open Ast_builder.Default;


let rec convert_nested_core_types = (core_type, type_name, opt, loc): label => {
    let core_type_string = Utils.extract_string_from_core_type(core_type, loc);
  /*   print_endline(Printf.sprintf("opt: %B", opt));
   */
    switch (type_name) {
    | "int"
    | "string"
    | "bool" => (type_name |> String.capitalize_ascii) ++ (opt ? "" : "!")
    | "option" =>
      convert_nested_core_types(
            List.mem(core_type_string, Constants.basic_types) ? core_type : Utils.get_core_type(core_type, loc) |> List.hd,
            core_type_string,
            true,
            loc,
          )
    | "list"
    | "array" =>
      "["
      ++ (
        convert_nested_core_types(
              List.mem(core_type_string, Constants.basic_types) ? core_type : Utils.get_core_type(core_type, loc) |> List.hd,
              core_type_string,
              false,
              loc,
            )
      )
      ++ "]" ++ (opt ? "" : "!")
    | _ => Location.raise_errorf(~loc, "Could not identify kind of type")
    };
  };

let convert_typename = (type_name, core_types, loc): label => {
    switch (core_types) {
    | [] =>
      List.mem(type_name, Constants.basic_types)
        ? (type_name |> String.capitalize_ascii) ++ "!"
        : (type_name ++ "_gql") ++ "!"
    | [core_type] => convert_nested_core_types(core_type, type_name, false, loc)
    | _ => Location.raise_errorf(~loc, "List of more than one core_type")
    };
  };

let get_type_correctly_formatted = (ptyp_desc, loc) => {
    switch (ptyp_desc) {
    | Ptyp_constr({txt: Lident(typename), _}, core_types) =>
      convert_typename(typename, core_types, loc)
    | _ => Location.raise_errorf(~loc, "Could not access typename")
    };
  };
  
let createSchemaField = (lds, loc): string => {
List.fold_left(
    (acc, ld) =>
    acc
    ++ " "
    ++ ld.pld_name.txt
    ++ ": "
    ++ get_type_correctly_formatted(ld.pld_type.ptyp_desc, loc)
    ++ "\n",
    "",
    lds,
);
};

let structure_item_with_basic_types =
    (lds: list(label_declaration), type_name, payload, loc): structure_item => {
  let type_ = type_name ++ "_gql";

  let gql_schema =
    "type "
    ++ ((payload == "" ? type_name : payload) |> String.capitalize_ascii)
    ++ " {\n"
    ++ createSchemaField(lds, loc)
    ++ "}";

  pstr_value(
    ~loc,
    Nonrecursive, 
    [
      {
        pvb_pat: ppat_var(~loc, {loc, txt: type_}),
        pvb_expr: {
          pexp_constant(~loc, Pconst_string(gql_schema, None));
        },
        pvb_attributes: [],
        pvb_loc: loc,
      },
    ],
  );
};