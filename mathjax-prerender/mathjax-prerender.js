/* Script from https://joa.sh/posts/2015-09-14-prerender-mathjax.html */

const mjpage = require("mathjax-node-page").mjpage;
const jsdom = require("jsdom");
const { JSDOM } = jsdom;
const fs = require("fs");
const path = require("path");

const MathJaxOptions = JSON.parse(fs.readFileSync("../templates/mathjax-options.js"));

var renderMathjaxForFile = (dir, fileName, callback) => {
    var fullPath = path.join(dir, fileName);
    var html = fs.readFile(fullPath, 'utf8', (err, data) => {
    var dom = new JSDOM(data);
    var document = dom.window.document;
    console.log("Rendering:", fileName);

    mjpage(document.body.innerHTML, {
      format: ["TeX"],
      fragment: true,
      displayErrors: true,
      MathJax: MathJaxOptions,
    }, {
      xmlns: "svg",
      svg: true
    }, function(result) {
      "use strict";
      document.body.innerHTML = result;
      var HTML = "<!DOCTYPE html>\n" 
        + document.documentElement.outerHTML
                  .replace(/^(\n|\s)*/, "");
      fs.writeFileSync(fullPath, HTML, 'utf8');
      callback();
    });
  });
};


var postDir = "../_site/posts/";

var posts = fs.readdirSync(postDir);

// Wait for all of these and the homepage, archives page
var pending = posts.length + 2;

var closeWhenDone = () => {
  pending -= 1;
  if (pending === 0) process.exit();
};

renderMathjaxForFile("../_site/", "index.html", closeWhenDone);
renderMathjaxForFile("../_site/", "archive.html", closeWhenDone);

posts.forEach(post => {
  renderMathjaxForFile(postDir, post, closeWhenDone);
});
