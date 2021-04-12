open RecTypes;
open Ppxlib;
open Ast_builder.Default;

module ExtractExpression = {
  type t =
    | Expression(expression)
    | ExpressionAndStructureItems(expression, list(structure_item));

  let map: (t, expression => expression) => t =
    (t, f) => {
      switch (t) {
      | Expression(expr) => Expression(f(expr))
      | ExpressionAndStructureItems(expr, structis) =>
        ExpressionAndStructureItems(f(expr), structis)
      };
    };

  let concat_expr: (expression, expression) => expression =
    (ea, eb) => {
      let loc = ea.pexp_loc; // NOTE: is this handled correctly?
      %expr
      [%e ea] ++ [%e eb];
    };

  let join: (t, t) => t =
    (a, b) => {
      switch (a, b) {
      | (Expression(ea), Expression(eb)) => Expression(concat_expr(ea, eb))
      | (Expression(ea), ExpressionAndStructureItems(eb, s))
      | (ExpressionAndStructureItems(ea, s), Expression(eb)) =>
        ExpressionAndStructureItems(concat_expr(ea, eb), s)
      | (
          ExpressionAndStructureItems(ea, sa),
          ExpressionAndStructureItems(eb, sb),
        ) =>
        ExpressionAndStructureItems(concat_expr(ea, eb), sa @ sb)
      };
    };

  let expr: t => expression =
    t =>
      switch (t) {
      | Expression(expr)
      | ExpressionAndStructureItems(expr, _) => expr
      };

  let structis: t => list(structure_item) =
    t =>
      switch (t) {
      | Expression(_) => []
      | ExpressionAndStructureItems(_, structis) => structis
      };
};

let alias_type_expression = (type_name, opt, loc, _isAbstr) => {
  switch%expr (
    [%e
      eapply(
        ~loc,
        evar(~loc, "Ppx_deriving_runtime.GraphQLppxSet.check_if_rec_type"),
        [
          evar(~loc, "__MODULE__"),
          estring(~loc, type_name |> String.capitalize_ascii),
        ],
      )
    ]
  ) {
  | true =>
    [%e
    estring(~loc, type_name |> String.capitalize_ascii)] ++ [%e estring(~loc, opt?"":"!")]
  | false => 
    if(
    ([%e eapply(
      ~loc,
      evar(~loc, "String.get"),
      [
        evar(~loc, type_name ++ "_gql"),
        [%expr -1 + [%e eapply(
          ~loc,
          evar(~loc, "String.length"),
          [
            evar(~loc, type_name ++ "_gql")
          ],
        )]], 
      ])]
    == '!') && [%e ebool(~loc, opt)]
    ) 
    {
      [%e eapply(
      ~loc,
      evar(~loc, "String.sub"),
      [
        evar(~loc, type_name ++ "_gql"),
        eint(~loc, 0),
        [%expr -1 + [%e eapply(
          ~loc,
          evar(~loc, "String.length"),
          [
            evar(~loc, type_name ++ "_gql")
          ],
        )]], 
      ])]
    } else {[%e evar(~loc, type_name ++ "_gql")]}
  };
};


let get_module_expression = (module_name, type_name, opt, loc): ExtractExpression.t => {

  let module_structur_item_list = [
    [%stri
        if([%e
          eapply(
            ~loc,
            evar(~loc, "Ppx_deriving_runtime.GraphQLppxSet.check_if_rec_type"),
            [
              estring(~loc, "Dune__exe__" ++ module_name),
              estring(~loc, type_name |> String.capitalize_ascii),
            ],
          )
        ]) {
          [%e
            eapply(
              ~loc,
              evar(~loc, "Ppx_deriving_runtime.GraphQLppxSet.add_rec_type"),
              [
                evar(~loc, "__MODULE__"),
                estring(
                  ~loc,
                  (module_name |> String.uncapitalize_ascii)
                  ++ "_"
                  ++ (type_name |> String.capitalize_ascii),
                ),
              ],
            )
          ]}
    ],
    [%stri
      let [%p
        pvar(
          ~loc,
          (module_name |> String.uncapitalize_ascii)
          ++ "_"
          ++ type_name
          ++ "_gql",
        )
      ] = [%e
        evar(~loc, module_name ++ "." ++ type_name ++ "_gql")
      ]
    ],
  ];

  let expression = if%expr (
    [%e
    eapply(
      ~loc,
      evar(~loc, "Ppx_deriving_runtime.GraphQLppxSet.check_if_rec_type"),
      [
        estring(~loc, "Dune__exe__" ++ module_name),
        estring(~loc, type_name |> String.capitalize_ascii),
      ],
    )]) {
      %e
      estring(
        ~loc,
        module_name ++ "_" ++ (type_name |> String.capitalize_ascii) ++ (opt ? "" : "!"),
      )
    } else if 
      ([%e
        eapply(
          ~loc,
          evar(~loc, "Ppx_deriving_runtime.GraphQLppxSet.check_if_rec_type"),
          [
            estring(~loc, "Dune__exe__" ++ module_name),
            eapply(
            ~loc,
            evar(~loc, "Str.global_replace"),
          [
            eapply(
            ~loc,
            evar(~loc, "Str.regexp"),
          [
            estring(~loc, "\\(\\[*\\)\\([A-Za-z_]+\\)\\(\\]\\|!\\)*"),
          ],
          ),
            estring(~loc, "\\2"),
            evar(~loc, module_name ++ "." ++ type_name ++ "_gql")
          ],
        ),
          ],
        )
      ]) { 
        let type_name_str_expr = [%e
            eapply(
            ~loc,
            evar(~loc, "Str.global_replace"),
          [
            eapply(
            ~loc,
            evar(~loc, "Str.regexp"),
          [
            estring(~loc, ("\\(\\.*\\)\\([A-Za-z_]+\\)\\(\\.*\\)") ),
          ]),
            estring(~loc, "\\1" ++ module_name ++ "_\\2"),
            evar(~loc, module_name ++ "." ++ type_name ++ "_gql")
          ],
        )
        ]; 
        [%e eapply(~loc, evar(~loc, "Ppx_deriving_runtime.GraphQLppxExpr.check_if_option"), [
          [%expr type_name_str_expr],
          ebool(~loc, opt)
        ])]
    } else {
      let type_name_str_expr = [%e evar(~loc, module_name ++ "." ++ type_name ++ "_gql")];
      [%e eapply(~loc, evar(~loc, "Ppx_deriving_runtime.GraphQLppxExpr.check_if_option"), [
          [%expr type_name_str_expr],
          ebool(~loc, opt)
        ])]
    };

  ExpressionAndStructureItems(expression, module_structur_item_list);
};

let rec extract_expression:
  (detectableTypes, ~isOptional: bool, ~loc: location, ~isAbstr: bool) => ExtractExpression.t =
  (generated_data, ~isOptional as opt, ~loc, ~isAbstr) => {
    switch (generated_data) {
    | Option(detectableTypes) =>
      extract_expression(detectableTypes, ~isOptional=true, ~loc, ~isAbstr)
    | Array(detectableTypes) =>
      let extractedType =
        extract_expression(detectableTypes, ~isOptional=false, ~loc, ~isAbstr);
      let makeExpr = expr => {
        %expr
        [%e estring(~loc, "[")]
        ++ [%e expr]
        ++ [%e estring(~loc, "]" ++ (opt ? "" : "!"))];
      };
      ExtractExpression.map(extractedType, makeExpr);
    | Module(module_name, type_name) =>
      get_module_expression(module_name, type_name, opt, loc)
    | Alias(type_name) => Expression(alias_type_expression(type_name, opt, loc, isAbstr))
    | String =>
      Expression([%expr [%e estring(~loc, "String" ++ (opt ? "" : "!"))]])
    | Integer =>
      Expression([%expr [%e estring(~loc, "Integer" ++ (opt ? "" : "!"))]])
    | Bool =>
      Expression([%expr [%e estring(~loc, "Bool" ++ (opt ? "" : "!"))]])
    | Record(_) =>
      Location.raise_errorf(~loc, "Type should not contain Record")
    };
  };


let label_expression = (recordItem, loc) => {

  let extracted_recordItem_type_expression =
    extract_expression(recordItem.type_, ~isOptional=false, ~loc, ~isAbstr=false);
  let record_field_name = " " ++ recordItem.propertyName ++ ": ";
  let makeExpr = expr => [%expr
    [%e estring(~loc, record_field_name)]
    ++ [%e expr]
    ++ [%e estring(~loc, "\n")]
  ];
  ExtractExpression.map(extracted_recordItem_type_expression, makeExpr);
};


let extractedExpressions = (generated_data, loc) =>
  switch (generated_data) {
  | Record(recordItems_list) =>
    recordItems_list
    |> List.tl
    |> List.fold_left(
         (acc, recordItem) => {
           ExtractExpression.join(acc, label_expression(recordItem, loc))
         },
         {
           label_expression(recordItems_list |> List.hd, loc);
         },
       )
  | _ => Location.raise_errorf(~loc, "error with generated_data")
  };



let create_structure_item_list = (loc, generated_data, type_name, name) => {

  let record_name_struct_item = [%stri
    [%e eapply(
      ~loc,
      evar(~loc, "Ppx_deriving_runtime.GraphQLppxSet.add_rec_type"),
      [
        evar(~loc, "__MODULE__"),
        estring(~loc, type_name),
      ],
    )]
  ];

  let expressions =
    ExtractExpression.expr(extractedExpressions(generated_data, loc));
  let structure_items =
    ExtractExpression.structis(extractedExpressions(generated_data, loc));
  let structure_items = [record_name_struct_item, ...structure_items];

  [
    [%stri
      let [%p pvar(~loc, type_name ++ "_gql")] =
        [%e
          estring(
            ~loc,
            "type "
            ++ ((name == "" ? type_name : name) |> String.capitalize_ascii)
            ++ " {\n",
          )
        ]
        ++ [%e expressions]
        ++ [%e estring(~loc, "}")]
    ],
    ...structure_items,
  ];
};


let create_abstract_structure_item = (loc, generated_data, type_name) => {
  let extracted_expr =
    extract_expression(generated_data, ~isOptional=false, ~loc, ~isAbstr=true);
  [
    [%stri
      let [%p pvar(~loc, type_name ++ "_gql")] = [%e
        ExtractExpression.expr(extracted_expr)
      ]
    ],
    ...ExtractExpression.structis(extracted_expr),
  ];
};