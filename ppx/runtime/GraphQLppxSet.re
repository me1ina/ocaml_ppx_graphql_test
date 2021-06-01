module StringSet = Set.Make(String);
module StringMap = Map.Make(String);
type rec_types_map = ref(StringMap.t(StringSet.t));
let rec_types:rec_types_map = ref(StringMap.empty);


let add_rec_type = (module_name, type_name) => {

    let str_set = 
    switch (StringMap.find_opt(module_name, rec_types^)) {
        | Some(set) => set 
        | _ => StringSet.empty
    }

    rec_types := rec_types^ |> StringMap.add(
        module_name,
        StringSet.add(
            type_name |> String.capitalize_ascii,
            str_set
        )
    )
};

let check_if_rec_type = (module_name, type_name) => {
    switch (StringMap.find_opt(module_name, rec_types^)) {
        | Some(set) => (set |> StringSet.mem(type_name |> String.capitalize_ascii)) ? true : false
        | _ => false
    }
    
};


/*   rec_types^ |> StringMap.iter((key, set) => {
    print_endline(key ++ "\n"); 
    set |> StringSet.iter(types => print_endline(types ++ "\n")); 
})
 
    print_endline("module:" ++ module_name);
    print_endline("type:" ++ type_name);  */

/*  let [%p pvar(~loc, "Ppx_deriving_runtime.GraphQLppxSet.rec_types")] = [%e
      eapply(
        ~loc,
        evar(~loc, "Ppx_deriving_runtime.GraphQLppxSet.StringMap.add"),
        [
          evar(~loc, "__MODULE__"),
          eapply(
            ~loc,
            evar(~loc, "Ppx_deriving_runtime.GraphQLppxSet.StringSet.add"),
            [
              estring(~loc, type_name |> String.capitalize_ascii),
              try(eapply(
                ~loc,
                evar(~loc, "Ppx_deriving_runtime.GraphQLppxSet.StringMap.find"),
                [
                  evar(~loc, "__MODULE__"),
                  evar(~loc, "Ppx_deriving_runtime.GraphQLppxSet.rec_types"),
                ],
              )) {
                | Not_found => evar(~loc, "Ppx_deriving_runtime.GraphQLppxSet.StringSet.empty")
              },
            ],
          ),
          evar(~loc, "Ppx_deriving_runtime.GraphQLppxSet.rec_types"),
        ],
      )
    ] */