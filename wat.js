
import { Qua } from "./vm.js"
import { parse_sexp } from "./parser.js"
if (node()) var fs = await import("fs") // ok with node 13.14 --harmony-top-level-await or greater and browser
export { Vm, readFile }

function Vm() {
	var qua = new Qua()
	qua.exec(parse_sexp(readFile("./boot.wat")))
    return {
        eval: function(exp) {
			return qua.exec(parse_sexp(exp))
        }
    };
};

function readFile(file) {
	if (fs) return fs.readFileSync(file, 'utf8') 
    var request = new XMLHttpRequest();
    request.open("get", file, false);
    request.send();
	return request.responseText
}

function node() {
	return new Function("try { return this===global } catch(e) { return false }")()
} 