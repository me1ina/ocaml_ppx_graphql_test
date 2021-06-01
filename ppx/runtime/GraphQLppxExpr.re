let check_if_option:(string, bool) => string = (type_name_str_expr, opt) => {
  let str_length = String.length(type_name_str_expr) - 1;
    if(String.get(type_name_str_expr, str_length) == '!' && opt)
      {
       String.sub(type_name_str_expr, 0, str_length)
      } else {
        type_name_str_expr
      }
  }
