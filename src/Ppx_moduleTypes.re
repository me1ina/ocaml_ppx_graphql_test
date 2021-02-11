open Ppxlib;
open Ast_builder.Default;

/*let module_type = (module_name, type_name, loc) => {
    %expr [%e estring(~loc, module_name ++ "_" ++ type_name)]
  
  }

let get_module_name = (core_type,loc) => {
  switch core_type {
    | Ptyp_constr({txt: Ldot(Lident(module_name), _), _}, _) => module_name
    | _ => Location.raise_errorf(~loc, "Could not access typename")
  };
}

let get_type_name = (core_type, loc) => {
  switch core_type {
    | Ptyp_constr({txt: Ldot(Lident(_), type_name), _}, _) => type_name
    | _ => Location.raise_errorf(~loc, "Could not access typename")
  };
}


let get_module_string_expr = (module_name, type_name, loc) => {

  print_endline("module_name: " ++ module_name);
  print_endline("type_name: " ++ type_name);

  %expr 
  List.mem(
    [%e estring(~loc, type_name |> String.capitalize_ascii)], 
    [%e evar(~loc, module_name ++ ".record_types")]) 

  ? [%e estring(~loc, module_name ++ "_" ++ type_name |> String.capitalize_ascii)]: 
  [%e evar(~loc, module_name ++ "." ++ type_name ++ "_gql")]
}


 let get_module_string = (core_type, loc) => {

  let module_name = List.nth(Utils.get_module(core_type, loc), 0);
  let type_name =List.nth(Utils.get_module(core_type, loc), 1);
  get_module_string_expr(module_name, type_name, loc);

  
} */


let get_module_expression_upacked = (module_name, type_name, loc) => {

  let expression = [%expr 
  List.mem(
    [%e estring(~loc, type_name |> String.capitalize_ascii)], 
    [%e evar(~loc, module_name ++ ".record_types")]) 
  ? 
    [%e estring(~loc, module_name ++ "_" ++ type_name |> String.capitalize_ascii)]
  : 
  [%e evar(~loc, module_name ++ "." ++ type_name ++ "_gql")]];

    let structur_item_list =  [[%stri let [%p pvar(~loc, "record_types")] = [%e evar(~loc, "record_types")] @ [[%e estring(~loc, (module_name |> String.uncapitalize_ascii) ++ "_" ++ type_name)]]],
      [%stri let [%p pvar(~loc, (module_name |> String.uncapitalize_ascii) ++ "_" ++ type_name ++ "_gql")] = [%e evar(~loc, module_name ++ "." ++ type_name ++ "_gql")]]]; 
  

  (expression, structur_item_list)
}

let get_module_expression = (core_type, loc) => {

  let module_name = List.nth(Utils.get_module(core_type, loc), 0);
  let type_name =List.nth(Utils.get_module(core_type, loc), 1);
  
  get_module_expression_upacked(module_name, type_name, loc); 
}
  
  /* module_type |> String.uncapitalize_ascii 
  
  [%e estring(~loc, type_name |> String.capitalize_ascii)] 
  
  [%stri let [%p pvar(~loc, "record_types")] = [%e evar(~loc, "record_types")] @ [[%e estring(~loc, module_name |> String.uncapitalize_ascii ++ "_" ++ type_name)]]],
    [%stri let [%p pvar(~loc, module_name |> String.uncapitalize_ascii ++ "_" ++ type_name ++ "_gql")] = [%e module_type_declaration(~loc, module_name, type_name ++ "_gql")] ], 
  
  
  */

/*   Ptyp_constr({txt: {li: Lident(modulename), s: typename}, _}, core_types) => print_endline("here") */