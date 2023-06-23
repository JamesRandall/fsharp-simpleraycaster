module App.Render

open Browser.Types
open Fable.Core
open Fable.Core.JsInterop
open App.Model

let toColorString color =
  let r = (color >>> 16) 
  let g= (color >>> 8) &&& 0xFF
  let b = color &&& 0xFF
  $"rgb({r},{g},{b})"
  
let adjustColor color value =
  let r = (color >>> 16) + value;
  let b = ((color >>> 8) &&& 0x00FF) + value;
  let g = (color &&& 0x0000FF) + value;
  g ||| (b <<< 8) ||| (r <<< 16);

let verticalLine (context:CanvasRenderingContext2D) color x startY endY =
  context.strokeStyle <- (color |> toColorString |> U3.Case1)
  context.lineWidth <- 1.
  context.beginPath()
  context.moveTo (x,startY)
  context.lineTo (x,endY)
  context.stroke()

let fill (context:CanvasRenderingContext2D) color left top width height =
  context.fillStyle <- (color |> toColorString |> U3.Case1)
  context.fillRect (left, top, width, height)
  
let clearCanvas (context:CanvasRenderingContext2D) =
  fill context 0 -1. -1. (context.canvas.width+2.) (context.canvas.height+2.)
  
let overlay (context:CanvasRenderingContext2D) =
  context.fillStyle <- ("rgba(0,0,0,0.66)" |> U3.Case1)
  context.fillRect (-1., -1., context.canvas.width+2., context.canvas.height+2.)
  
let fillText (context:CanvasRenderingContext2D) text x y =
  context.textAlign <- "start"
  context.fillStyle <- ("#e0e0e0" |> U3.Case1)
  context.fillText (text,x,y)
  
let centerText (context:CanvasRenderingContext2D) text offsetX offsetY =
  context.textAlign <- "center"
  context.fillStyle <- ("#e0e0e0" |> U3.Case1)
  context.fillText (text, context.canvas.width/2. + offsetX, context.canvas.height/2. - 15. + offsetY)
  
let stroke (context:CanvasRenderingContext2D) strokeSize color x1 y1 x2 y2 =
  context.strokeStyle <- (color |> toColorString |> U3.Case1)
  context.lineWidth <- strokeSize
  context.beginPath()
  context.moveTo (x1,y1)
  context.lineTo (x2,y2)
  context.stroke()
  
let strokeSingle (context:CanvasRenderingContext2D) color x1 y1 x2 y2 =
  stroke context 1. color x1 y1 x2 y2
  
