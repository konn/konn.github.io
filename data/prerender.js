const jsdom = require('jsdom');
const {JSDOM} = jsdom;
const dom = new JSDOM(require('fs').readFileSync('/dev/stdin', 'utf8'));
document = dom.window.document;
const renderMathInElement = require('auto-render');
renderMathInElement(document.body, {
    delimiters:
    [ {left: "\\(", right: "\\)", display: false },
      {left: "\\[", right: "\\]", display: true }
    ]
});
process.stdout.write(dom.serialize());
