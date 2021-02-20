open RecTypes;
open Ppxlib;
open Ast_builder.Default;

let structure_item_list = ref([]); 

let alias_type_expression = (type_name, loc) => {
    [%expr 
      List.mem(
        [%e estring(~loc, type_name |> String.capitalize_ascii)], 
        [%e evar(~loc, "record_types")]) 
      ? [%e estring(~loc, type_name |> String.capitalize_ascii)] : 
      [%e evar(~loc, type_name ++ "_gql")]];   
}

let get_module_expression = (module_name, type_name, loc) => {

    let module_structur_item_list =  [[%stri let [%p pvar(~loc, "record_types")] = 
        List.mem(
            [%e estring(~loc, type_name |> String.capitalize_ascii)], 
            [%e evar(~loc, module_name ++ ".record_types")]) 
        ? ([%e evar(~loc, "record_types")] @ [[%e estring(~loc, (module_name |> String.uncapitalize_ascii) ++ "_" ++ type_name)]]) : [%e evar(~loc, "record_types")]],
            [%stri let [%p pvar(~loc, (module_name |> String.uncapitalize_ascii) ++ "_" ++ type_name ++ "_gql")] = [%e evar(~loc, module_name ++ "." ++ type_name ++ "_gql")]]]; 

    structure_item_list := structure_item_list^ @ module_structur_item_list;
    
    [%expr 
    List.mem(
      [%e estring(~loc, type_name |> String.capitalize_ascii)], 
      [%e evar(~loc, module_name ++ ".record_types")]) 
    ? 
      [%e estring(~loc, module_name ++ "_" ++ type_name |> String.capitalize_ascii)]
    : 
    [%e evar(~loc, module_name ++ "." ++ type_name ++ "_gql")]];

  }

let rec extract_expression = (generated_data, opt, loc) => {

    switch (generated_data) {
        | Option(detectableTypes) =>
            extract_expression(detectableTypes, true, loc)
        | Array(detectableTypes) => {
          [%expr
          [%e estring(~loc, "[")]
          ++ (
            [%e extract_expression(detectableTypes, false, loc)]
          )
          ++ [%e estring(~loc, "]" ++ (opt ? "" : "!"))]];
        }
        | Module(module_name, type_name) => get_module_expression(module_name, type_name, loc);
        | Alias(type_name) => alias_type_expression(type_name, loc);
        | String => %expr [%e estring(~loc, "String" ++ (opt ? "" : "!"))];
        | Integer => %expr [%e estring(~loc, "Integer" ++ (opt ? "" : "!"))];
        | Bool => %expr [%e estring(~loc, "Bool" ++ (opt ? "" : "!"))];
        | Record(_) => Location.raise_errorf(~loc, "Type should not contain Record")
        };
};

let create_structure_item_list = (loc, generated_data, type_name, name) => {

  print_endline("type_name: " ++ type_name);

  let record_name_struct_item = [[%stri let [%p pvar(~loc, "record_types")] = [%e evar(~loc, "record_types")] @ [[%e estring(~loc, type_name |> String.capitalize_ascii)]]]];
  structure_item_list := structure_item_list^ @ record_name_struct_item;
    
    let expressions = 
        switch generated_data {
        | Record(recordItems_list) => {
            recordItems_list 
                |> List.tl 
                |> List.fold_left(
                    (acc, recordItem) => {
                    let extracted_recordItem_type_expression = extract_expression(recordItem.type_, false, loc);
                    let record_field_name = " " ++ recordItem.propertyName ++ ": ";
                    let label_expression = 
                        [%expr
                        [%e estring(~loc, record_field_name)]
                        ++ [%e extracted_recordItem_type_expression]
                        ++ [%e estring(~loc, "\n")]];
                    [%expr
                    [%e acc] ++ [%e label_expression]];
                    },
                    {
                    let recordItem = recordItems_list |> List.hd;
                    let extracted_recordItem_type_expression = extract_expression(recordItem.type_, false, loc);
                    let record_field_name = " " ++ recordItem.propertyName ++ ": ";
                    let label_expression = 
                        [%expr
                        [%e estring(~loc, record_field_name)]
                        ++ [%e extracted_recordItem_type_expression]
                        ++ [%e estring(~loc, "\n")]];
                    [%expr [%e label_expression]];
                },
              );

        }
        | _ => Location.raise_errorf(~loc, "error with generated_data")
        };

    [[%stri
        let [%p pvar(~loc, type_name ++ "_gql")] =
          [%e estring(~loc, "type " ++ ((name == "" ? type_name : name) |> String.capitalize_ascii) ++ " {\n")]
          ++ [%e
          expressions
          ]
          ++ [%e estring(~loc, "}")]
      ]] @ structure_item_list^;

}

let create_abstract_structure_item = (loc, generated_data, type_name) => {
  [
    [%stri
      let [%p pvar(~loc, type_name ++ "_gql")] = [%e
      extract_expression(generated_data, false, loc)
    ]
    ]
  ];

}