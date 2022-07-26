var VM = require("./vm.js")
var boot_bytecode = require("./build/boot.js").main
var parser = require("./parser.js")
//var boot_bytecode = parser.parse_sexp(require('fs').readFileSync('./boot.wat', 'utf8'))

var vm = new VM()
vm.exec(boot_bytecode)

module.exports.vm = function() {
    return {
        eval: function(sexp) {
			var e = parser.parse_sexp(sexp)
			//console.log(vm.e)
			return vm.exec(e)
        }
    };
};
