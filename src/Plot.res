module DOM = Webapi.Dom
module Doc = Webapi.Dom.Document
module Elem = Webapi.Dom.Element
module EvtTarget = Webapi.Dom.EventTarget
module Canvas = Webapi.Canvas
module CanvasElement = Webapi.Canvas.CanvasElement
module C2d = Webapi.Canvas.Canvas2d

@bs.val external requestAnimationFrame: ('a => unit) => unit = "requestAnimationFrame"
@bs.val external document: {..} = "document"

type polar = (float, float) // (radius, theta)
type cartesian = (float, float) // (0.0, 0.0) is at center
type canvasCoord = (float, float) // (0.0, 0.0) is at top left

let toRadians = (degrees: float): float => {
  degrees *. Js.Math._PI /. 180.0
}

let toCartesian = ((r, theta): polar): cartesian => {
  (r *. cos(toRadians(theta)), r *. sin(toRadians(theta)))
}

let rec gcd = (m, n) => {
  if m === n {
    m
  } else if m > n {
    gcd(m -. n, n)
  } else {
    gcd(m, n -. m)
  }
}

let lcm = (m, n) => {
  m *. n /. gcd(m, n)
}

let plot = (
  formula1: DomGraphs.formula,
  formula2: DomGraphs.formula,
  plotAs: DomGraphs.graphType,
): unit => {
  let element = document["getElementById"]("canvas")
  let context = element["getContext"]("2d")
  let width = Belt.Float.fromInt(element["width"])
  let height = Belt.Float.fromInt(element["height"])
  let centerX = width /. 2.0
  let centerY = height /. 2.0

  C2d.setFillStyle(context, String, "white")
  context["fillRect"](~x=0.0, ~y=0.0, ~w=width, ~h=height)

  let amplitude = Js.Math.max_float(1.0, abs_float(formula1.factor) +. abs_float(formula2.factor))

  let toCanvas = ((x, y): cartesian): cartesian => {
    (centerX /. amplitude *. x +. centerX, -.centerY /. amplitude *. y +. centerY)
  }

  let evaluate = (f: DomGraphs.formula, angle: float): float => {
    f.factor *. f.fcn(f.theta *. toRadians(angle) +. toRadians(f.offset))
  }

  let getPolar = (theta): cartesian => {
    let r1 = evaluate(formula1, theta)
    let r2 = evaluate(formula2, theta)
    toCartesian((r1 +. r2, theta))
  }

  let getLissajous = (theta): cartesian => {
    let r1 = evaluate(formula1, theta)
    let r2 = evaluate(formula2, theta)
    (r1, r2)
  }

  let drawLines = (getXY: float => cartesian): unit => {
    let increment = 3.0
    let limit = 360.0 *. lcm(formula1.theta, formula2.theta)
    let rec helper = (d: float) => {
      if d >= limit {
        ()
      } else {
        let (x, y) = toCanvas(getXY(d))
        context["lineTo"](~x, ~y)
        helper(d +. increment)
      }
    }
    let (x, y) = toCanvas(getXY(0.0))
    C2d.setStrokeStyle(context, String, "#000")
    context["beginPath"]()
    context["moveTo"](~x, ~y)
    helper(increment)
    context["closePath"]()
    context["stroke"]()
    ()
  }

  // draw axes
  C2d.setStrokeStyle(context, String, "#999")
  context["beginPath"]()
  context["moveTo"](~x=0.0, ~y=centerY)
  context["lineTo"](~x=width, ~y=centerY)
  context["moveTo"](~x=centerX, ~y=0.0)
  context["lineTo"](~x=centerX, ~y=height)
  context["closePath"]()
  context["stroke"]()

  // draw the plot lines
  drawLines(plotAs == Polar ? getPolar : getLissajous)
}

let rec draw = _evt => {
  let formula1 = DomGraphs.getFormula("1")
  let formula2 = DomGraphs.getFormula("2")
  let plotAs = DomGraphs.getTypeOfGraph()
  plot(formula1, formula2, plotAs)

  requestAnimationFrame(() => {
    draw()
  })
}

draw()

/*
    polar: function(angle, f1, f2) {
        var r = f1 + f2;
        this.xPos = this.xBase + (r * Math.cos(this.angle * Math.PI / 180) / this.scaleMax) * this.length;
        this.yPos = this.yBase - (r * Math.sin(this.angle * Math.PI / 180) / this.scaleMax) * this.length;
    },

    lissajous: function(angle, f1, f2) {
        var r = f1 + f2;
        this.xPos = this.xBase + (f1 / this.scaleMax) * this.length;
        this.yPos = this.yBase - (f2 / this.scaleMax) * this.length;
    }
 */
