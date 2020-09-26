// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");
var DomGraphs$Domgraphs = require("./DomGraphs.bs.js");
var Webapi__Canvas__Canvas2d = require("bs-webapi/src/Webapi/Canvas/Webapi__Canvas__Canvas2d.js");

function radians(degrees) {
  return degrees * Math.PI / 180.0;
}

function toCartesian(param) {
  var theta = param[1];
  var r = param[0];
  return [
          r * Math.cos(radians(theta)),
          r * Math.sin(radians(theta))
        ];
}

function gcd(_m, _n) {
  while(true) {
    var n = _n;
    var m = _m;
    if (m === n) {
      return m;
    }
    if (m > n) {
      _m = m - n | 0;
      continue ;
    }
    _n = n - m | 0;
    continue ;
  };
}

function lcm(m, n) {
  return Caml_int32.div(Math.imul(m, n), gcd(m, n));
}

function lcm_float(m, n) {
  return lcm(m * 100.0 | 0, n * 100.0 | 0) / 100.0;
}

function plot(formula1, formula2, plotAs) {
  var element = document.getElementById("canvas");
  var context = element.getContext("2d");
  var width = element.width;
  var height = element.height;
  var centerX = width / 2.0;
  var centerY = height / 2.0;
  Webapi__Canvas__Canvas2d.setFillStyle(context, /* String */0, "white");
  context.fillRect(0.0, 0.0, width, height);
  var amplitude = Math.max(1.0, Math.abs(formula1.factor) + Math.abs(formula2.factor));
  var toCanvas = function (param) {
    return [
            centerX / amplitude * param[0] + centerX,
            -centerY / amplitude * param[1] + centerY
          ];
  };
  var evaluate = function (f, angle) {
    return f.factor * Curry._1(f.fcn, f.theta * radians(angle) + radians(f.offset));
  };
  var getPolar = function (theta) {
    var r1 = evaluate(formula1, theta);
    var r2 = evaluate(formula2, theta);
    return toCartesian([
                r1 + r2,
                theta
              ]);
  };
  var getLissajous = function (theta) {
    var r1 = evaluate(formula1, theta);
    var r2 = evaluate(formula2, theta);
    return [
            r1,
            r2
          ];
  };
  Webapi__Canvas__Canvas2d.setStrokeStyle(context, /* String */0, "#999");
  context.beginPath();
  context.moveTo(0.0, centerY);
  context.lineTo(width, centerY);
  context.moveTo(centerX, 0.0);
  context.lineTo(centerX, height);
  context.closePath();
  context.stroke();
  var getXY = plotAs === /* Polar */0 ? getPolar : getLissajous;
  var limit = 360.0 * lcm_float(formula1.theta, formula2.theta);
  var helper = function (_d) {
    while(true) {
      var d = _d;
      if (d >= limit) {
        return ;
      }
      var match = toCanvas(Curry._1(getXY, d));
      context.lineTo(match[0], match[1]);
      _d = d + 3.0;
      continue ;
    };
  };
  var match = toCanvas(Curry._1(getXY, 0.0));
  Webapi__Canvas__Canvas2d.setStrokeStyle(context, /* String */0, "#000");
  context.beginPath();
  context.moveTo(match[0], match[1]);
  helper(3.0);
  context.closePath();
  context.stroke();
  
}

function draw(_evt) {
  var formula1 = DomGraphs$Domgraphs.getFormula("1");
  var formula2 = DomGraphs$Domgraphs.getFormula("2");
  var plotAs = DomGraphs$Domgraphs.getTypeOfGraph(undefined);
  plot(formula1, formula2, plotAs);
  requestAnimationFrame(function (param) {
        return draw(undefined);
      });
  
}

draw(undefined);

var DOM;

var Doc;

var Elem;

var EvtTarget;

var Canvas;

var CanvasElement;

var C2d;

exports.DOM = DOM;
exports.Doc = Doc;
exports.Elem = Elem;
exports.EvtTarget = EvtTarget;
exports.Canvas = Canvas;
exports.CanvasElement = CanvasElement;
exports.C2d = C2d;
exports.radians = radians;
exports.toCartesian = toCartesian;
exports.gcd = gcd;
exports.lcm = lcm;
exports.lcm_float = lcm_float;
exports.plot = plot;
exports.draw = draw;
/*  Not a pure module */
