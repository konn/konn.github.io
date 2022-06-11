import { readFileSync } from "fs";
import { JSDOM } from "jsdom";
const dom = new JSDOM(readFileSync("/dev/stdin", "utf8"));
global.document = dom.window.document;

import renderMathInElement from "katex/contrib/auto-render";

try {
  renderMathInElement(document.body, {
    trust: true,
    throwOnError: false,
    delimiters: [
      { left: "\\(", right: "\\)", display: false },
      { left: "\\[", right: "\\]", display: true },
    ],
  });
} finally {
  process.stdout.write(dom.serialize());
}
