let e = epsilon_float;

let nan = () => {
  Alcotest.(check @@ float(e))("NaN is NaN", nan, nan);
  Alcotest.(check @@ neg @@ float(e))("NaN is not number", nan, 7.);
  Alcotest.(check @@ neg @@ float(e))("number is not NaN", 8., nan);
};

let infinity = () => {
  Alcotest.(check @@ float(e))("+∞ is +∞", infinity, infinity);
  Alcotest.(check @@ float(e))("-∞ is -∞", neg_infinity, neg_infinity);
  Alcotest.(check @@ neg @@ float(e))(
    "+∞ is not -∞",
    infinity,
    neg_infinity,
  );
  Alcotest.(check @@ neg @@ float(e))(
    "-∞ is not +∞",
    neg_infinity,
    infinity,
  );
  Alcotest.(check @@ neg @@ float(e))("+∞ is not 3", infinity, 3.);
};

let others = () => {
  Alcotest.(check @@ float(e))("0 is 0", 0., 0.);
  Alcotest.(check @@ float(e))("0 is epsilon", 0., e);
  Alcotest.(check @@ neg @@ float(e))("0 is not 1", 0., 1.);
  Alcotest.(check @@ neg @@ float(e))("1 is not 0", 1., 0.);
  Alcotest.(check @@ float(e))(".3 is .3", 0.1 +. 0.2, 0.4);
};

let edge_set = [("NaN", `Quick, nan), ("∞", `Quick, infinity)];

let others_set = [("others", `Quick, others)];

let () =
  Alcotest.run(
    "Simple tests",
    [("Edge cases", edge_set), ("Other floats", others_set)],
  ); 
