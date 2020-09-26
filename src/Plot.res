@bs.val external requestAnimationFrame: ('a => unit) => unit = "requestAnimationFrame"
@bs.val external document: {..} = "document"

type polar = {radius: float, theta: float}
type cartesian = {cartesianX: float, cartesianY: float}
type canvasCoord = {canvasX: float, canvasY: float}

let toRadians = (degrees: float): float => {
  degrees *. Js.Math._PI /. 180.0
}

let toCartesian = ({radius: r, theta}) => {
  {
    cartesianX: r *. cos(toRadians(theta)),
    cartesianY: r *. sin(toRadians(theta)),
  }
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

let rec draw = _evt => {
  let formula1 = DomGraphs.getFormula("1")
  let formula2 = DomGraphs.getFormula("2")
  let plotAs = DomGraphs.getTypeOfGraph()

  let element = document["getElementById"]("canvas")
  let context = element["getContext"]("2d")
  let width = Belt.Float.fromInt(element["width"])
  let height = Belt.Float.fromInt(element["height"])
  let centerX = width /. 2.0
  let centerY = height /. 2.0

  context["fillStyle"] = "white"
  context["fillRect"](~x=0.0, ~y=0.0, ~w=width, ~h=height)

  let amplitude = Js.Math.max_float(1.0, abs_float(formula1.factor) +. abs_float(formula2.factor))

  let toCanvas = ({cartesianX: x, cartesianY: y}) => {
    {
      canvasX: centerX /. amplitude *. x +. centerX,
      canvasY: -.centerY /. amplitude *. y +. centerY,
    }
  }

  let evaluate = (f: DomGraphs.formula, angle) => {
    f.factor *. f.fcn(f.theta *. toRadians(angle) +. toRadians(f.offset))
  }

  let getPolar = theta => {
    let r1 = evaluate(formula1, theta)
    let r2 = evaluate(formula2, theta)
    toCartesian({radius: r1 +. r2, theta: theta})
  }

  let getLissajous = theta => {
    let r1 = evaluate(formula1, theta)
    let r2 = evaluate(formula2, theta)
    {cartesianX: r1, cartesianY: r2}
  }

  // draw axes
  context["strokeStyle"] = "#999"
  context["beginPath"]()
  context["moveTo"](~x=0.0, ~y=centerY)
  context["lineTo"](~x=width, ~y=centerY)
  context["moveTo"](~x=centerX, ~y=0.0)
  context["lineTo"](~x=centerX, ~y=height)
  context["closePath"]()
  context["stroke"]()

  // draw the plot lines
  let getXY = plotAs == Polar ? getPolar : getLissajous
  let increment = 3.0
  let {canvasX, canvasY} = toCanvas(getXY(0.0))
  context["strokeStyle"] = "#000"
  context["beginPath"]()
  context["moveTo"](canvasX, canvasY)
  let d = ref(increment)
  let limit = 360.0 *. lcm(formula1.theta, formula2.theta)
  while d.contents < limit {
    let {canvasX, canvasY} = toCanvas(getXY(d.contents))
    context["lineTo"](canvasX, canvasY)
    d.contents = d.contents +. increment
  }
  context["closePath"]()
  context["stroke"]()

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
