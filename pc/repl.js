// REPL for SpiderMonkey command line tool
// Contributed by Chris Neukirchen
// Adapted by mjs

// In same directory as repl.js:
// > js repl.js
//
// Or provide files at command line:
// > js repl.js ../test.wat
//
// Use -m for JIT.

load("../jsparse.js");
load("../wat.js");

var wat_env = wat.mkenvcore();

var console = { log: print };

function wat_console_log(string) {
    console.log(string);
}

function wat_eval(str) {
    var forms = wat.parse(str);
    var start = new Date().getTime();
    for (var i = 0; i < forms.length; i++) {
        var result = wat.eval(forms[i], wat_env);
        wat_console_log(result.toSource());
    }
    var elapsed = new Date().getTime() - start;
    wat_console_log("Evaluation time " + elapsed + "ms");
}

function wat_load_file(path) {
    wat_console_log("Loading " + path + "...");
    wat_eval(read(path));
}

wat_load_file("../crust.wat");
wat_load_file("pc.wat");
wat_load_file("../repl.wat");
arguments.forEach(wat_load_file);
