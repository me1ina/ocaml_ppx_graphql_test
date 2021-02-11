open Ppxlib;
open Ast_builder.Default;

/* ----------- abstract types ----------- */

let get_abstract_expression = (core_type, loc) => {
    Utils.isBasicTypeRec(core_type.ptyp_desc, loc)
      ? estring(~loc, Ppx_basicTypes.get_type_correctly_formatted(core_type.ptyp_desc, loc))
      : {
        let extracted_core_type = Utils.extract_string_from_core_type(core_type, loc);
        evar(~loc, extracted_core_type ++ "_gql");
      }
  };
  
let abstract_accessor_impl = (core_type, type_name, loc):list(structure_item) => 
    [
    [%stri
      let [%p pvar(~loc, type_name ++ "_gql")] = [%e
      get_abstract_expression(core_type, loc)
    ]
    ]
  ];

/* ----------- record types ------------- */

let rec resolve_string = (lds, loc): string => {
    let ld = List.hd(lds);
  
    Utils.isBasicTypeRec(ld.pld_type.ptyp_desc, loc)
      ? Ppx_basicTypes.createSchemaField([ld], loc)
        ++ (
          compare(List.length(lds), 1) == 0
            ? "" : resolve_string(List.tl(lds), loc)
        )
      : "";
};

let resolve_first_part = (lds, typename, payload, loc): string => {
    let recString = resolve_string(lds, loc);

    "type " ++ ((payload == "" ? typename : payload) |> String.capitalize_ascii) ++ " {\n" ++ recString;
};
  
let rec get_record_list_without_first_basic_records =
          (lds, loc): list(label_declaration) => {
        Utils.isBasicTypeRec(List.hd(lds).pld_type.ptyp_desc, loc) && List.length(lds) > 1
      ? get_record_list_without_first_basic_records(List.tl(lds), loc) : lds;
  };
  
let check_if_record_expression = (type_name, loc) => {  
    let expression = 
    [%expr 
      List.mem(
        [%e estring(~loc, type_name |> String.capitalize_ascii)], 
        [%e evar(~loc, "record_types")]) 
      ? [%e estring(~loc, type_name |> String.capitalize_ascii)] : 
      [%e evar(~loc, type_name ++ "_gql")]]; 

    (expression, []);
      
}

let rec convert_complex_nested_core_types = (core_type, type_name, opt, loc) => {
    if(Utils.isModuleType(core_type, loc)){

     Ppx_moduleTypes.get_module_expression(core_type, loc);

    }
    else {
      let core_type_string = Utils.extract_string_from_core_type(core_type, loc);
      let core_type_list = Utils.get_core_type(core_type, loc);
  
      switch (type_name) {
      | "option" =>
        convert_complex_nested_core_types(
              List.length(core_type_list) > 0 ? core_type_list |> List.hd : core_type,
              core_type_string,
              true,
              loc,
            )
      | "list"
      | "array" =>{
        let (rec_expr, rec_structure_items) = convert_complex_nested_core_types(
          List.length(core_type_list) > 0 ? core_type_list |> List.hd : core_type,
          core_type_string,
          false,
          loc,
        );
        let expression = 
        [%expr
        [%e estring(~loc, "[")]
        ++ (
          [%e rec_expr]
        )
        ++ [%e estring(~loc, "]" ++ (opt ? "" : "!"))]]; 

        (expression, rec_structure_items);
      
      }
      | _ => check_if_record_expression(type_name, loc)
      };}
};
  
let complexGqlDeclaration = (ld, loc) => {
    let (expr, structure_items) =
      (
        switch (ld.pld_type.ptyp_desc) {
        | Ptyp_constr({txt: Lident(typename), _}, core_types) => {
          core_types == [] ? { check_if_record_expression(typename, loc) } : convert_complex_nested_core_types(core_types |> List.hd, typename, false, loc)
        }
        | Ptyp_constr({txt: Ldot(Lident(module_name), typename), _}, _) => {
          
          Ppx_moduleTypes.get_module_expression_upacked(module_name, typename, loc);
        
        }
        | _ => Location.raise_errorf(~loc, "Coud not access typename")
        }
      );
    let record_field_name = " " ++ ld.pld_name.txt ++ ": ";
  
    let expression =
    [%expr
    [%e estring(~loc, record_field_name)]
    ++ [%e expr]
    ++ [%e estring(~loc, "\n")]];

    (expression, structure_items);
};
  
let primitiveGqlDeclaration = (ld, loc) => {
    let expr_string = Ppx_basicTypes.createSchemaField([ld], loc);
    (estring(~loc, expr_string), []);
};
  
let structure_item_with_alias_types = (lds, typename, payload, loc): list(structure_item) => {
 
  let make_expression = ld =>
    Utils.isBasicTypeRec(ld.pld_type.ptyp_desc, loc)
        ? primitiveGqlDeclaration(ld, loc) : {
          complexGqlDeclaration(ld, loc)
        };
  
    let consolidate_expressions = expressions =>
      expressions
      |> List.tl
      |> List.fold_left(
           (acc, ld) => {
            let (expr, structure_items) = make_expression(ld);
            let expression = [%expr
             [%e acc |> fst] ++ [%e expr]];
            let str_items = (acc |> snd) @ structure_items;
            (expression, str_items);
           },
           {
            let (expr, structure_items) = make_expression(expressions |> List.hd);
            ([%expr [%e expr]], structure_items);
          },
         );

      let (expr, structure_items) = consolidate_expressions(get_record_list_without_first_basic_records(lds, loc));
  
      [[%stri
        let [%p pvar(~loc, typename ++ "_gql")] =
          [%e estring(~loc, resolve_first_part(lds, typename, payload, loc))]
          ++ [%e
            expr
          ]
          ++ [%e estring(~loc, "}")]
      ]] @ structure_items;
};