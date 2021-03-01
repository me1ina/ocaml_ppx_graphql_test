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

let alias_type_expression = (type_name, loc) => {
  switch%expr (
    [%e
      eapply(
        ~loc,
        evar(~loc, "StringSet.mem"),
        [
          estring(~loc, type_name |> String.capitalize_ascii),
          evar(~loc, "rec_types"),
        ],
      )
    ]
  ) {
  | true =>
    %e
    estring(~loc, type_name |> String.capitalize_ascii)
  | false =>
    %e
    evar(~loc, type_name ++ "_gql")
  };
};

let get_module_expression = (module_name, type_name, loc): ExtractExpression.t => {
  let module_structur_item_list = [
    [%stri
      let [%p pvar(~loc, "rec_types")] =
        [%e
          eapply(
            ~loc,
            evar(~loc, "StringSet.mem"),
            [
              estring(~loc, type_name |> String.capitalize_ascii),
              evar(~loc, module_name ++ ".rec_types"),
            ],
          )
        ]
          ? [%e
            eapply(
              ~loc,
              evar(~loc, "StringSet.add"),
              [
                estring(
                  ~loc,
                  (module_name |> String.uncapitalize_ascii)
                  ++ "_"
                  ++ type_name,
                ),
                evar(~loc, "rec_types"),
              ],
            )
          ]
          : [%e evar(~loc, "rec_types")]
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

  let expression =
    switch%expr (
      [%e
        eapply(
          ~loc,
          evar(~loc, "StringSet.mem"),
          [
            estring(~loc, type_name |> String.capitalize_ascii),
            evar(~loc, module_name ++ ".rec_types"),
          ],
        )
      ]
    ) {
    | true =>
      %e
      estring(
        ~loc,
        module_name ++ "_" ++ type_name |> String.capitalize_ascii,
      )
    | false =>
      [%e
        eapply(
          ~loc,
          evar(~loc, "StringSet.mem"),
          [
            evar(~loc, module_name ++ "." ++ type_name ++ "_gql"),
            evar(~loc, module_name ++ ".rec_types"),
          ],
        )
      ]
        ? [%e estring(~loc, module_name ++ "_")]
          ++ [%e evar(~loc, module_name ++ "." ++ type_name ++ "_gql")]
        : [%e evar(~loc, module_name ++ "." ++ type_name ++ "_gql")]
    };

  ExpressionAndStructureItems(expression, module_structur_item_list);
};

let rec extract_expression:
  (detectableTypes, ~isOptional: bool, ~loc: location) => ExtractExpression.t =
  (generated_data, ~isOptional as opt, ~loc) => {
    switch (generated_data) {
    | Option(detectableTypes) =>
      extract_expression(detectableTypes, ~isOptional=true, ~loc)
    | Array(detectableTypes) =>
      let extractedType =
        extract_expression(detectableTypes, ~isOptional=false, ~loc);
      let makeExpr = expr => {
        %expr
        [%e estring(~loc, "[")]
        ++ [%e expr]
        ++ [%e estring(~loc, "]" ++ (opt ? "" : "!"))];
      };
      ExtractExpression.map(extractedType, makeExpr);
    | Module(module_name, type_name) =>
      get_module_expression(module_name, type_name, loc)
    | Alias(type_name) => Expression(alias_type_expression(type_name, loc))
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
    extract_expression(recordItem.type_, ~isOptional=false, ~loc);
  let record_field_name = " " ++ recordItem.propertyName ++ ": ";
  let makeExpr = expr => [%expr
    [%e estring(~loc, record_field_name)]
    ++ [%e expr /*extracted_recordItem_type_expression*/]
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
    let [%p pvar(~loc, "rec_types")] = [%e
      eapply(
        ~loc,
        evar(~loc, "StringSet.add"),
        [
          estring(~loc, type_name |> String.capitalize_ascii),
          evar(~loc, "rec_types"),
        ],
      )
    ]
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
    extract_expression(generated_data, ~isOptional=false, ~loc);
  [
    [%stri
      let [%p pvar(~loc, type_name ++ "_gql")] = [%e
        ExtractExpression.expr(extracted_expr)
      ]
    ],
    ...ExtractExpression.structis(extracted_expr),
  ];
};