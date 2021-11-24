import "./style.css";
import main from "../output/Main";
import "../output/Components.Editor";
import "../output/Components.Player";
import "../node_modules/ace-builds/src-noconflict/ace.js";
import "../node_modules/ace-builds/src-noconflict/mode-haskell";
import "../node_modules/ace-builds/src-noconflict/theme-cobalt";
main.main();

if (module.hot) {
	module.hot.accept("../output/Main", function () {
		document.body.innerHTML = "";
		main.main();
	});
	module.hot.accept("../output/Components.Player", function () {
		document.body.innerHTML = "";
		main.main();
	});
	module.hot.accept("../output/Components.Editor", function () {
		document.body.innerHTML = "";
		main.main();
	});
}
