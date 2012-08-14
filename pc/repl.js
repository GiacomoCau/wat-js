// REPL for SpiderMonkey command line tool
// Contributed by Chris Neukirchen
// Adapted by mjs

load("jsparse.js");
load("altwat.js");

var wat_env = wat.mkenvcore();

var console = { log: print };

function wat_console_log(string) {
    console.log(string);
}

function wat_eval(str) {
    var start = new Date().getTime();
    var forms = wat.parse(str);
    elapsed = new Date().getTime() - start;
    wat_console_log("Parse time " + elapsed + "ms");
    start = new Date().getTime();
    for (var i = 0; i < forms.length; i++) {
        var result = wat.eval(forms[i], wat_env);
    }
    elapsed = new Date().getTime() - start;
    wat_console_log("Evaluation time " + elapsed + "ms");
}

function wat_load_file(path) {
    wat_console_log("Loading " + path + "...");
    wat_eval(read(path));
}

wat_load_file("../crust.wat");
//wat_load_file("pc.wat");
//arguments.forEach(wat_load_file);
//wat_load_file("../repl.wat");
