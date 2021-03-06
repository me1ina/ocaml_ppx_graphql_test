// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Path = require("path");
var Curry = require("bs-platform/lib/js/curry.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");
var CodeFrame = require("@babel/code-frame");

var match = typeof __dirname === "undefined" ? undefined : __dirname;

var dirname = match !== undefined ? match : "";

function cleanUpStackTrace(stack) {
  var removeInternalLines = function (lines, _i) {
    while(true) {
      var i = _i;
      if (i >= lines.length) {
        return lines;
      } else if (lines[i].indexOf(" (internal/") >= 0) {
        return lines.slice(0, i);
      } else {
        _i = i + 1 | 0;
        continue ;
      }
    };
  };
  return removeInternalLines(stack.split("\\n").slice(2), 0).map((function (line) {
                  return line.slice(2);
                })).join("\\n");
}

function run(loc, left, comparator, right) {
  if (Curry._2(comparator, left, right)) {
    return 0;
  } else {
    var match = loc[0];
    var line = match[1];
    var file = match[0];
    var fileContent = Fs.readFileSync(Path.join(dirname, file), {
          encoding: "utf-8"
        });
    var left$1 = JSON.stringify(left);
    var right$1 = JSON.stringify(right);
    var codeFrame = CodeFrame.codeFrameColumns(fileContent, {
          start: {
            line: line
          }
        }, {
          highlightCode: true
        });
    var errorMessage = "\n  \\u001b[31mTest Failure!\n  \\u001b[36m" + (String(file) + ("\\u001b[0m:\\u001b[2m" + (String(line) + ("\n" + (String(codeFrame) + ("\n  \\u001b[39mLeft: \\u001b[31m" + (String(left$1 === undefined ? undefined : Caml_option.some(left$1)) + ("\n  \\u001b[39mRight: \\u001b[31m" + (String(right$1 === undefined ? undefined : Caml_option.some(right$1)) + "\\u001b[0m\n")))))))));
    console.log(errorMessage);
    var obj = { };
    Error.captureStackTrace(obj);
    console.log(cleanUpStackTrace(obj.stack));
    return /* () */0;
  }
}

exports.dirname = dirname;
exports.cleanUpStackTrace = cleanUpStackTrace;
exports.run = run;
/* match Not a pure module */
