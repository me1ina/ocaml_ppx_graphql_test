let check_if_option:(string, bool) => string = (type_name_str_expr, opt) => {
  print_endline(opt?"true":"false");
  let str_length = String.length(type_name_str_expr) - 1;
    if(String.get(type_name_str_expr, str_length) == '!' && opt)
      {
       String.sub(type_name_str_expr, 0, str_length)
      } else {
        type_name_str_expr
      }
  }



  let str:(string, bool) => string = (str, _bool) => {
    str
  }

/*   if%expr (
    ([%e eapply(
      ~loc,
      evar(~loc, "String.get"),
      [
        estring(~loc, type_name_str_expr),
        [%expr -1 + [%e eapply(
          ~loc,
          evar(~loc, "String.length"),
          [
            estring(~loc, type_name_str_expr)
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
        estring(~loc, type_name_str_expr),
        eint(~loc, 0),
        [%expr -1 + [%e eapply(
          ~loc,
          evar(~loc, "String.length"),
          [
            estring(~loc, type_name_str_expr)
          ],
        )]], 
      ])]
    } else {
      estring(~loc, type_name_str_expr)
    } */