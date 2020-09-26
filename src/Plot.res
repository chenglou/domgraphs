@bs.val external requestAnimationFrame: ('a => unit) => unit = "requestAnimationFrame"

type polar = {radius: float, theta: float}
type cartesian = {cartesianX: float, cartesianY: float}
type canvasCoord = {canvasX: float, canvasY: float}

// greatest common denominator
let rec gcd = (m, n) => {
  if m === n {
    m
  } else if m > n {
    gcd(m -. n, n)
  } else {
    gcd(m, n -. m)
  }
}

// least common multiple
let lcm = (m, n) => {
  m *. n /. gcd(m, n)
}

let context = DomGraphs.canvas["getContext"]("2d")
let canvasWidth = Belt.Float.fromInt(DomGraphs.canvas["width"])
let canvasHeight = Belt.Float.fromInt(DomGraphs.canvas["height"])
let centerX = canvasWidth /. 2.0
let centerY = canvasHeight /. 2.0

let toRadians = degrees => {
  degrees *. Js.Math._PI /. 180.0
}
let toCartesian = ({radius: r, theta}) => {
  {
    cartesianX: r *. cos(toRadians(theta)),
    cartesianY: r *. sin(toRadians(theta)),
  }
}
let toCanvas = ({cartesianX: x, cartesianY: y}, ~amplitude) => {
  {
    canvasX: centerX /. amplitude *. x +. centerX,
    canvasY: -.centerY /. amplitude *. y +. centerY,
  }
}

@warning("-21")
let rec draw = _evt => {
  let formula1 = DomGraphs.getFormula("1")
  let formula2 = DomGraphs.getFormula("2")

  let amplitude = Js.Math.max_float(
    1.0,
    Js.Math.abs_float(formula1.factor) +. Js.Math.abs_float(formula2.factor),
  )

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

  // wipe screen
  context["fillStyle"] = "white"
  context["fillRect"](~x=0.0, ~y=0.0, ~w=canvasWidth, ~h=canvasHeight)

  // draw axes
  context["strokeStyle"] = "#999"
  context["beginPath"]()
  context["moveTo"](~x=0.0, ~y=centerY)
  context["lineTo"](~x=canvasWidth, ~y=centerY)
  context["moveTo"](~x=centerX, ~y=0.0)
  context["lineTo"](~x=centerX, ~y=canvasHeight)
  context["closePath"]()
  context["stroke"]()

  // draw the plot lines
  let getXY = DomGraphs.getTypeOfGraph() == Polar ? getPolar : getLissajous
  let increment = 1.0
  context["strokeStyle"] = "#000"
  context["lineWidth"] = 0.5
  context["beginPath"]()
  let {canvasX, canvasY} = toCanvas(getXY(0.0), ~amplitude)
  context["moveTo"](canvasX, canvasY)
  let d = ref(increment)
  let limit = 360.0 *. lcm(formula1.theta, formula2.theta)
  while d.contents < limit {
    let {canvasX, canvasY} = toCanvas(getXY(d.contents), ~amplitude)
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
