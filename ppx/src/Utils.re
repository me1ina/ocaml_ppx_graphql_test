open Ppxlib;

let extract_string_from_core_type = (core_type, loc) => {
    switch (core_type.ptyp_desc) {
    | Ptyp_constr({txt: Lident(typename), _}, _) => typename
    | _ => Location.raise_errorf(~loc, "Could not extract string from core type")
    };
  };

let get_core_type = (core_type, loc) => {
    switch (core_type.ptyp_desc) {
    | Ptyp_constr({txt: Lident(_), _}, core_types) => core_types
    | _ => Location.raise_errorf(~loc, "No core_types found")
    };
};

let isModuleType =  (core_type, loc) => {
    switch (core_type.ptyp_desc) {
    | Ptyp_constr({txt: Lident(_), _}, _) => false
    | Ptyp_constr({txt: Ldot(Lident(_), _), _}, _) => true
    | _ => Location.raise_errorf(~loc, "Could not identify kind of type")
    };
  };

let get_module = (core_type, loc) => {
    switch (core_type.ptyp_desc) {
    | Ptyp_constr({txt: Ldot(Lident(module_name), type_name), _}, _) => [|module_name,type_name|]
    | _ => Location.raise_errorf(~loc, "Could not access module")
    };
};

let isBasicType = (typename): bool =>
  List.mem(typename, Constants.basic_types) ? true : false;

let rec isBasicTypeRec = (ptyp_desc, loc): bool =>
  switch (ptyp_desc) {
  | Ptyp_constr({txt: Lident(typename), _}, core_types) =>
    core_types == []
      ? isBasicType(typename)
      : core_types
        |> List.fold_left(
             (acc, core_type) =>
               acc && isBasicTypeRec(core_type.ptyp_desc, loc),
             true,
           )
  | Ptyp_constr({txt: Ldot(Lident(_), _), _}, _) => false
  | _ => Location.raise_errorf(~loc, "Element is no ptyp_desc")
  };


let rec kindOfTypeRec = (ptyp_desc, loc) =>
  switch (ptyp_desc) {
  | Ptyp_constr({txt: Lident(typename), _}, core_types) =>
    core_types == []
      ? (isBasicType(typename) ? "Ppx_basicTypes" : "Ppx_aliasTypes")
      : kindOfTypeRec((core_types |> List.hd).ptyp_desc, loc)
  | Ptyp_constr({txt: Ldot(Lident(_), _), _}, _) => "Ppx_moduleTypes"
  | _ => Location.raise_errorf(~loc, "Element is no ptyp_desc")
};

