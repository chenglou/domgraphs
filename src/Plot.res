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
  let context = CanvasElement.getContext2d(element)
  let width = Belt.Float.fromInt(CanvasElement.width(element))
  let height = Belt.Float.fromInt(CanvasElement.height(element))
  let centerX = width /. 2.0
  let centerY = height /. 2.0

  C2d.setFillStyle(context, String, "white")
  C2d.fillRect(~x=0.0, ~y=0.0, ~w=width, ~h=height, context)

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
        C2d.lineTo(~x, ~y, context)
        helper(d +. increment)
      }
    }
    let (x, y) = toCanvas(getXY(0.0))
    C2d.setStrokeStyle(context, String, "#000")
    C2d.beginPath(context)
    C2d.moveTo(context, ~x, ~y)
    helper(increment)
    C2d.closePath(context)
    C2d.stroke(context)
  }

  // draw axes
  C2d.setStrokeStyle(context, String, "#999")
  C2d.beginPath(context)
  C2d.moveTo(context, ~x=0.0, ~y=centerY)
  C2d.lineTo(context, ~x=width, ~y=centerY)
  C2d.moveTo(context, ~x=centerX, ~y=0.0)
  C2d.lineTo(context, ~x=centerX, ~y=height)
  C2d.closePath(context)
  C2d.stroke(context)

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
