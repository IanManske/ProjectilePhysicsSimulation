module ProjectilePhysicsSimulation.Graphics

open Fable.Core
open Browser.Dom
open Browser.Types

open Physics

let width = 1000
let height = 600

let private getCanvasAndContext id =
  let canvas = document.getElementById id :?> HTMLCanvasElement
  canvas, canvas.getContext_2d()

let trajectoryCanvas, trajectoryContext = getCanvasAndContext "trajectoryCanvas"
let bodyCanvas, bodyContext = getCanvasAndContext "bodyCanvas"
let velocityCanvas, velocityContext = getCanvasAndContext "velocityCanvas"

// HTMLCanvas uses top-left corner as (0,0) with positive y meaning downwards on the screen.
// Translate the canvas so that (0,0) is at least the bottom left corner (still have to multiply y-coordinate by -1 when drawing):
let private initCanvasWith width height (canvas: HTMLCanvasElement) (context: CanvasRenderingContext2D) =
  canvas.width <- float width
  canvas.height <- float height
  context.translate(0.0, float height)

// Initialize canvases
let private initCanvas = initCanvasWith width height

initCanvas trajectoryCanvas trajectoryContext
initCanvas bodyCanvas bodyContext
initCanvas velocityCanvas velocityContext

trajectoryContext.fillStyle <- U3.Case1 "black"
bodyContext.fillStyle <- U3.Case1 "blue"
velocityContext.strokeStyle <- U3.Case1 "red"



// Draw functions:
let clearVelocityMarker projectile =
  let c = Body.center projectile
  velocityContext.clearRect(
    float c.X - float (abs projectile.Velocity.X) - 1.0,
    -float c.Y - float (abs projectile.Velocity.Y) - 1.0,
    float (2.0 * abs projectile.Velocity.X) + 2.0,
    float (2.0 * abs projectile.Velocity.Y) + 2.0)

let drawVelocityMarker projectile =
  velocityContext.beginPath()
  let c = Body.center projectile
  velocityContext.moveTo(float c.X, -float c.Y)
  velocityContext.lineTo(
    float c.X + float projectile.Velocity.X,
    -float c.Y - float projectile.Velocity.Y)
  velocityContext.closePath()
  velocityContext.stroke()


let clearBody projectile =
  bodyContext.clearRect(
    float projectile.Position.X - 1.0,
    -float projectile.Position.Y - float projectile.Height - 1.0,
    float projectile.Width + 2.0,
    float projectile.Height + 2.0)

let drawBody projectile =
  bodyContext.fillRect(
    float projectile.Position.X,
    -float projectile.Position.Y - float projectile.Height,
    float projectile.Width,
    float projectile.Height)


let drawTracer (point: Vector2<_>) =
  trajectoryContext.beginPath()
  trajectoryContext.arc(
    float point.X,
    -float point.Y,
    2.5,
    0.0,
    System.Math.PI * 2.0)
  trajectoryContext.closePath()
  trajectoryContext.fill()


let clear (context: CanvasRenderingContext2D) =
  context.clearRect(0.0, -context.canvas.height, context.canvas.width, context.canvas.height)

let clearTrajectoryContext () = clear trajectoryContext
let clearVelocityContext () = clear velocityContext
let clearBodyContext () = clear bodyContext
