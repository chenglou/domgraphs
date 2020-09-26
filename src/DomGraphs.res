module DOM = Webapi.Dom // use all upper case to distinguish from built-in Dom
module Doc = Webapi.Dom.Document
module Elem = Webapi.Dom.Element
module HtmlElem = Webapi.Dom.HtmlElement
module InputElem = Webapi.Dom.HtmlInputElement
module EvtTarget = Webapi.Dom.EventTarget
module Result = Belt.Result

type trigFcn = float => float

type formula = {
  factor: float,
  fcn: trigFcn,
  theta: float,
  offset: float,
}

type graphType =
  | Polar
  | Lissajous

let userInfo = ref((
  {factor: 1.0, fcn: sin, theta: 1.0, offset: 0.0},
  {factor: 1.0, fcn: sin, theta: 1.0, offset: 0.0},
))

external unsafeAsHtmlInputElement: Elem.t => InputElem.t = "%identity"

type numResult = Result.t<float, string>
type fcnResult = Result.t<trigFcn, string>
type formulaResult = Result.t<formula, string>
type inputResult<'a> = Result.t<'a, string>

let getInputValue = (id: string, default: 'a, f: (string, 'a) => inputResult<'a>): inputResult<
  'a,
> => {
  switch Doc.getElementById(id, DOM.document) {
  | Some(element) => {
      let s = InputElem.value(unsafeAsHtmlInputElement(element))
      f(s, default)
    }
  | None => Result.Error("No element" ++ id)
  }
}

/*
 * Get text from the given id (unless it doesn't exist, in which case
 *   return Error("no such id")
 * If the string is empty, return Ok(default).
 * If non-empty and conversion to float is successful, return Ok(value)
 * otherwise, return Error("non-numeric")
 */

let getNumericValue = (id: string, default: float): numResult => {
  let converter = (s: string, default: float): numResult => {
    if s == "" {
      Result.Ok(default)
    } else {
      switch float_of_string(s) {
      | result => Result.Ok(result)
      | exception Failure(_) => Result.Error(s ++ " is not numeric.")
      }
    }
  }
  getInputValue(id, default, converter)
}

let getFunctionValue = (id: string): fcnResult => {
  let converter = (s: string, _default: trigFcn): fcnResult => {
    if s == "sin" {
      Result.Ok(sin)
    } else if s == "cos" {
      Result.Ok(cos)
    } else {
      Result.Error("Unknown trig function " ++ s)
    }
  }
  getInputValue(id, sin, converter) // include a default to make getInputValue happy
}

let multiMap = (
  rX: Result.t<'a, 'b>, // this will be our formula
  rY: Result.t<'c, 'b>, // this is the result of getting an input field
  f: ('c, 'a) => 'a,
): // function to insert input value ('c) into formula ('a)
Result.t<'a, 'b> => {
  switch (rX, rY) {
  | (Result.Ok(x), Result.Ok(y)) => Result.Ok(f(y, x))
  | (Result.Ok(_x), Result.Error(err)) => (Result.Error(err): Result.t<'a, 'b>)
  | (err, _) => err
  }
}

let getFormula = (suffix: string): Result.t<formula, string> => {
  let possibleFormula = Result.Ok({factor: 1.0, fcn: sin, theta: 1.0, offset: 0.0})

  multiMap(possibleFormula, getNumericValue("factor" ++ suffix, 1.0), (factor, form) => {
    ...form,
    factor: factor,
  })
  ->multiMap(getFunctionValue("fcn" ++ suffix), (fcn, formula) => {...formula, fcn: fcn})
  ->multiMap(getNumericValue("theta" ++ suffix, 1.0), (theta, formula) => {
    ...formula,
    theta: theta,
  })
  ->multiMap(getNumericValue("offset" ++ suffix, 0.0), (offset, formula) => {
    ...formula,
    offset: offset,
  })
}

let getRadioValue = (radioButtons: array<(string, 'a)>, default: 'a) => {
  let rec helper = (index: int) => {
    if index == Belt.Array.length(radioButtons) {
      default
    } else {
      switch Doc.getElementById(fst(radioButtons[index]), DOM.document) {
      | Some(element) => {
          let input = unsafeAsHtmlInputElement(element)
          if InputElem.checked(input) {
            snd(radioButtons[index])
          } else {
            helper(index + 1)
          }
        }
      | None => helper(index + 1)
      }
    }
  }
  helper(0)
}
