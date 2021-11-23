import "./style.css";
import "./prism.css";
import "./prism.js";
import main from "../output/Main";
import "../output/Components.Editor";
import "../output/Components.Player";
import { defineCustomElements as deckDeckGoElement } from "@deckdeckgo/highlight-code/dist/loader";
deckDeckGoElement();
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
