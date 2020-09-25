module DOM = Webapi.Dom
module Doc = Webapi.Dom.Document
module Elem = Webapi.Dom.Element
module EvtTarget = Webapi.Dom.EventTarget
module Canvas = Webapi.Canvas
module CanvasElement = Webapi.Canvas.CanvasElement
module C2d = Webapi.Canvas.Canvas2d

@bs.val external setTimeoutFloat : ('a => unit, float, 'a) => int = "setTimeout";

type polar = (float, float) // (radius, theta)
type cartesian = (float, float) // (0.0, 0.0) is at center
type canvasCoord = (float, float) // (0.0, 0.0) is at top left

let radians = (degrees: float): float => {
  degrees *. Js.Math._PI /. 180.0
}

let toCartesian = ((r, theta): polar): cartesian => {
  (r *. cos(radians(theta)), r *. sin(radians(theta)))
}

let rec gcd = (m: int, n:int): int => {
  if (m == n) {
    m
  } else if (m > n) {
    gcd(m - n, n)
  } else {
    gcd(m, n - m)
  }
}

let lcm = (m: int, n: int): int => {
  (m * n) / gcd(m, n)
}

let lcm_float = (m:float, n:float): float => {
  float_of_int(lcm(int_of_float(m *. 100.0),
    int_of_float(n *. 100.0))) /. 100.0
}

let plot = (formula1: DomGraphs.formula, formula2: DomGraphs.formula,
  plotAs: DomGraphs.graphType):unit => {
  
  switch (Doc.getElementById("canvas", DOM.document)) {
    | Some(element) => {
        let context = CanvasElement.getContext2d(element);
        let width = float_of_int(CanvasElement.width(element));
        let height = float_of_int(CanvasElement.height(element));
        let centerX = width /. 2.0;
        let centerY = height /. 2.0;

        C2d.setFillStyle(context, String, "white");
        C2d.fillRect(~x=0.0, ~y=0.0, ~w=width, ~h=height, context);

        let amplitude = Js.Math.max_float(1.0, abs_float(formula1.factor)
          +. abs_float(formula2.factor))
                
        let toCanvas = ((x, y): cartesian): cartesian => {
          ((centerX /. amplitude) *. x +. centerX,
          (-.centerY /. amplitude) *.y +. centerY)
        }
                
        let evaluate = (f: DomGraphs.formula, angle: float): float => {
          f.factor *. f.fcn(f.theta *. radians(angle) +. radians(f.offset))
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
        
        let drawLines = (getXY: (float)=>cartesian): unit => {
          let increment = 3.0
          let limit = 360.0 *. lcm_float(formula1.theta, formula2.theta)
          let rec helper = (d: float) => {
            if (d >= limit) {
              ()
            } else {
              let (x, y) = toCanvas(getXY(d))
              C2d.lineTo(~x = x, ~y = y, context)
              helper(d +. increment)
            }
          }
          let (x, y) = toCanvas(getXY(0.0))
          C2d.setStrokeStyle(context, String, "#000")
          C2d.beginPath(context)
          C2d.moveTo(context, ~x=x, ~y=y)
          helper(increment)
          C2d.closePath(context)
          C2d.stroke(context)
        }
          

        // draw axes
        C2d.setStrokeStyle(context, String, "#ccc")
        C2d.beginPath(context)
         Belt.Array.forEach([50.0, 100.0, 150.0, 200.0],
            (item) => {C2d.moveTo(context, ~x=item +. centerX, ~y=centerY);
              C2d.arc(context, ~x=centerX, ~y=centerY,
              ~r=item, ~startAngle=0.0, ~endAngle=Js.Math._PI *. 2.0, ~anticw=false)});
          Belt.Array.forEach([0.0, 30.0, 60.0, 90.0, 120.0, 150.0],
            (item) => {
              let (x, y) = toCanvas(toCartesian((amplitude, item)));
              Js.log2(x, y)
              C2d.moveTo(context, ~x=x, ~y=y);
              C2d.lineTo(context, ~x=width -. x , ~y=height -. y);
            });   
          //C2d.moveTo(context, ~x=centerX, ~y=centerY);
          //C2d.lineTo(context, ~x=centerX, ~y=centerY);
          C2d.closePath(context); 

        C2d.stroke(context)
        
        // draw the plot lines
        drawLines((plotAs == Polar) ? getPolar : getLissajous)

      }
    | None => ()
  }
}

let draw = (_evt) => {
  let formula1 = DomGraphs.getFormula("1")
  let formula2 = DomGraphs.getFormula("2")
  let plotAs = DomGraphs.getRadioValue([("polar", DomGraphs.Polar),
    ("lissajous", DomGraphs.Lissajous)], DomGraphs.Polar)
  switch (formula1, formula2) {
    | (Belt.Result.Ok(f1), Belt.Result.Ok(f2)) => {
        Js.log2("formula 1:", f1)
        Js.log2("formula 2:", f2)
        Js.log2("plot as: ", plotAs)
        plot(f1, f2, plotAs)
      }
    | (Belt.Result.Error(e1), _) => DOM.Window.alert(e1, DOM.window)
    | (_, Belt.Result.Error(e2)) => DOM.Window.alert(e2, DOM.window)
  }
}

let optButton = Doc.getElementById("draw", DOM.document)
switch (optButton) {
  | Some(button) => {
      EvtTarget.addClickEventListener(draw, Elem.asEventTarget(button))
    }
  | None => DOM.Window.alert("Cannot find button", DOM.window)
}

          
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
