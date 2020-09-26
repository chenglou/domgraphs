@bs.val external document: {..} = "document"

type formula = {
  factor: float,
  fcn: float => float,
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

let getNumericValue = (domId, default) => {
  switch document["getElementById"](domId)["value"] {
  | "" => default
  | stringValue =>
    switch Belt.Float.fromString(stringValue) {
    | None => default
    | Some(value) => value
    }
  }
}

let getFunctionValue = (domId) => {
  if document["getElementById"](domId)["value"] === "sin" {
    sin
  } else {
    cos
  }
}

let getFormula = (suffix) => {
  {
    factor: getNumericValue("factor" ++ suffix, 1.0),
    fcn: getFunctionValue("fcn" ++ suffix),
    theta: getNumericValue("theta" ++ suffix, 1.0),
    offset: getNumericValue("offset" ++ suffix, 0.0),
  }
}

let getTypeOfGraph = () => {
  if document["getElementById"]("polar")["checked"] {
    Polar
  } else {
    Lissajous
  }
}
