module AddRecTypeTest = {
    let type_to_add = "Type0";
    Ppx_deriving_runtime.GraphQLppxSet.add_rec_type(__MODULE__, type_to_add);
    let test_gql = 
    switch(Ppx_deriving_runtime.GraphQLppxSet.StringMap.find_opt(__MODULE__, Ppx_deriving_runtime.GraphQLppxSet.rec_types^)) {
        | Some(item) => item |> Ppx_deriving_runtime.GraphQLppxSet.StringSet.find(type_to_add)
        | _ => "error: type not added correctly"
    };
    let name = "AddRecTypeTest";
    let expected =
"Type0";
};

    module CheckIfRecTypeTest = {
    let rec_type1 = "Type1";
    Ppx_deriving_runtime.GraphQLppxSet.add_rec_type(__MODULE__, rec_type1);
    let test_gql = 
    switch(Ppx_deriving_runtime.GraphQLppxSet.check_if_rec_type(__MODULE__, rec_type1)) {
        | true => "true"
        | _ => "error: type not checked correctly"
    };
    let name = "CheckIfRecTypeTest";
    let expected =
"true";
};