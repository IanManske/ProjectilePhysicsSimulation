module ProjectilePhysicsSimulation.Graphics

open System
open Physics

open Browser.Dom
open Browser.Types

let getCanvasAndContext id =
  let canvas = document.getElementById id :?> HTMLCanvasElement
  canvas, canvas.getContext_2d()

let trajectoryCanvas, trajectoryContext = getCanvasAndContext "trajectoryCanvas"
let bodyCanvas, bodyContext = getCanvasAndContext "bodyCanvas"
let velocityCanvas, velocityContext = getCanvasAndContext "velocityCanvas"

// HTMLCanvas uses top-left corner as (0,0) with positive y meaning downwards on the screen.
// Translate the canvas so that (0,0) is at least the bottom left corner (still have to multiply y-coordinate by -1 when drawing):
let initCanvasWith width height (canvas: HTMLCanvasElement) (context: CanvasRenderingContext2D) =
  canvas.width <- float width
  canvas.height <- float height
  context.translate(0.0, float height)

// Draw functions:
let clearVelocityMarker projectile =
  let c = center projectile
  velocityContext.clearRect(
    float c.X - float (abs projectile.Velocity.X) - 1.0,
    -float c.Y - float (abs projectile.Velocity.Y) - 1.0,
    float (2.0 * abs projectile.Velocity.X) + 2.0,
    float (2.0 * abs projectile.Velocity.Y) + 2.0)

let drawVelocityMarker projectile =
  velocityContext.beginPath()
  let c = center projectile
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

let drawTracer point =
  trajectoryContext.beginPath()
  trajectoryContext.arc(
    float point.X,
    -float point.Y,
    2.5,
    0.0,
    Math.PI * 2.0)
  trajectoryContext.closePath()
  trajectoryContext.fill()

let clear (context: CanvasRenderingContext2D) =
  context.clearRect(0.0, -context.canvas.height, context.canvas.width, context.canvas.height)

let simulateAndDraw model totalTime =
  clearBody model.Projectile
  clearVelocityMarker model.Projectile

  if not model.Started then
    drawTracer <| center model.Projectile

  let newModel, tracers = simulate model totalTime

  drawBody newModel.Projectile
  drawVelocityMarker newModel.Projectile

  List.iter drawTracer tracers

  newModel