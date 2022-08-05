
import { Qua } from "./vm.js"
import { parse_sexp } from "./parser.js"
//import bootWat from "./boot.wat"
export { Vm }

function Vm() {
	var qua = new Qua()
	qua.exec(parse_sexp(readFile("./boot.wat")))
	//qua.exec(parse_sexp(bootWat))
    return {
        eval: function(exp) {
			return qua.exec(parse_sexp(exp))
        }
    };
};

function readFile(file) {
    var request = new XMLHttpRequest();
    request.open("get", file, false);
    request.send();
	return request.responseText
}
