/* jshint node: true */
"use strict";

var gulp = require("gulp");
var jshint = require("gulp-jshint");
var jscs = require("gulp-jscs");
var purescript = require("gulp-purescript");
var rimraf = require("rimraf");

var sources = [
  "src/**/*.purs",
  "bower_components/purescript-*/src/**/*.purs"
];

var foreigns = [
  "src/**/*.js",
  "bower_components/purescript-*/src/**/*.js"
];

gulp.task("clean-docs", function (cb) {
  rimraf("docs", cb);
});

gulp.task("clean-output", function (cb) {
  rimraf("output", cb);
});

gulp.task("clean", ["clean-docs", "clean-output"]);

gulp.task("lint", function() {
  return gulp.src("src/**/*.js")
    .pipe(jshint())
    .pipe(jshint.reporter())
    .pipe(jscs());
});

gulp.task("make", ["lint"], function() {
  return purescript.psc({ src: sources, ffi: foreigns });
});

gulp.task("docs", ["clean-docs"], function() {
  return purescript.pscDocs({
    src: sources,
    docgen: {
      "Halogen": "docs/Halogen.md",
      "Halogen.Component": "docs/Halogen-Component.md",
      "Halogen.HTML.Events.Types": "docs/Halogen-Events.md",
      "Halogen.HTML.Events": "docs/Halogen-Events.md",
      "Halogen.HTML.Events.Handler": "docs/Halogen-Events.md",
      "Halogen.HTML.Events.Monad": "docs/Halogen-Events.md",
      "Halogen.HTML.Events.Forms": "docs/Halogen-Forms.md",
      "Halogen.HTML.CSS": "docs/Halogen-HTML-CSS.md",
      "Halogen.HTML.Renderer.VirtualDOM": "docs/Halogen-HTML-Renderer.md",
      "Halogen.HTML.Renderer.String": "docs/Halogen-HTML-Renderer.md",
      "Halogen.HTML": "docs/Halogen-HTML.md",
      "Halogen.Mixin.Router": "docs/Halogen-Mixin-Router.md",
      "Halogen.Mixin.UndoRedo": "docs/Halogen-Mixin-UndoRedo.md",
      "Halogen.Signal": "docs/Halogen-Signal.md",
      "Halogen.HTML.Target": "docs/Halogen-Target.md"
    }
  });
});

gulp.task("dotpsci", function () {
  return purescript.psci({ src: sources, ffi: foreigns })
    .pipe(gulp.dest("."));
});

gulp.task("default", ["make", "docs", "dotpsci"]);
