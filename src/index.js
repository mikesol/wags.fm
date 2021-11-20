import "./style.css";
import "./prism.css";
import "./prism.js";
import main from "../output/Main/index.js";
import { defineCustomElements as deckDeckGoElement } from "@deckdeckgo/highlight-code/dist/loader";
deckDeckGoElement();

main.main();
