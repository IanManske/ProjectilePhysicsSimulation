module ProjectilePhysicsSimulation.Graphics

open Fable.Core
open Browser.Dom
open Browser.Types

open Physics

let private phi = (1.0 + sqrt 5.0) / 2.0
let width = window.screen.availWidth / phi
let height = window.screen.availHeight * 0.75

let private setupCanvas id =
  let canvas = document.getElementById id :?> HTMLCanvasElement
  canvas.width <- width
  canvas.height <- height

  // HTMLCanvas uses top-left corner as (0,0) with
  // positive y meaning downwards on the screen.
  // Translate and scale the canvas so that (0,0)
  // is the bottom left corner with positive y meaning upwards:
  let context = canvas.getContext_2d()
  context.translate(0.0, height)
  context.scale(1.0, -1.0)
  canvas, context

let trajectoryCanvas, trajectoryContext = setupCanvas "trajectoryCanvas"
let bodyCanvas, bodyContext = setupCanvas "bodyCanvas"
let velocityCanvas, velocityContext = setupCanvas "velocityCanvas"

trajectoryContext.fillStyle <- U3.Case1 "black"
bodyContext.fillStyle <- U3.Case1 "blue"
velocityContext.strokeStyle <- U3.Case1 "red"




let private clear (context: CanvasRenderingContext2D) =
  context.clearRect(0.0, 0.0, context.canvas.width, context.canvas.height)

let drawTracer (point: Vector2<_>) =
  trajectoryContext.beginPath()
  trajectoryContext.arc(
    float point.X,
    float point.Y,
    2.5,
    0.0,
    System.Math.PI * 2.0)
  trajectoryContext.closePath()
  trajectoryContext.fill()

(*
One formula for linear interpolation:
  let lerp a b alpha = (1 - alpha) * a + alpha * b
The phyisics simulation works by calculating changes in position.
Substituting in (a + delta) for b, a simplification occurs:
  lerp position newPosition alpha
  = lerp position (position + deltaPosition) alpha
  = (1 - alpha) * position + alpha * (position + deltaPosition)
  = position - alpha * position + alpha * position + alpha * deltaPosition
  = position + alpha * deltaPosition
*)
let private lerpDelta (Vector2(x, y)) (Vector2(dx, dy)) alpha = Vector2(x + alpha * dx, y + alpha * dy)
// let private lerpDelta (a: Vector2<_>) (delta: Vector2<_>) (alpha: float) = a + alpha * delta

let maybeDrawTracer traceInterval (timeSinceLastTracer: float<Data.UnitSystems.SI.UnitSymbols.s>) =
  let mutable tracerCount = 1
  let mutable nextTracerTime = traceInterval - timeSinceLastTracer

  fun projectile deltaPosition time ->
    if time >= nextTracerTime then
      // alpha: how far into the timeStep the tracer should be drawn. [ 0 = start of timeStep, 1 = end of timeStep ]
      ((timeStep - (time - nextTracerTime)) / timeStep) // alpha
      |> lerpDelta (Body.center projectile) deltaPosition // alpha -> tracerPosition
      |> drawTracer // tracerPosition -> draw
      tracerCount <- tracerCount + 1
      nextTracerTime <- traceInterval * float tracerCount - timeSinceLastTracer

let resetTrajectory projectile =
  clear trajectoryContext
  drawTracer <| Body.center projectile




let clearBody projectile =
  bodyContext.clearRect(
    float projectile.Position.X - 1.0,
    float projectile.Position.Y - 1.0,
    float projectile.Width + 2.0,
    float projectile.Height + 2.0)

let drawBody projectile =
  bodyContext.fillRect(
    float projectile.Position.X,
    float projectile.Position.Y,
    float projectile.Width,
    float projectile.Height)


let clearVelocityMarker projectile =
  let c = Body.center projectile
  let v = projectile.Velocity
  velocityContext.clearRect(
    float c.X - float (abs v.X) - 1.0,
    float c.Y - float (abs v.Y) - 1.0,
    float (2.0 * abs v.X) + 2.0,
    float (2.0 * abs v.Y) + 2.0)

let drawVelocityMarker projectile =
  velocityContext.beginPath()
  let c = Body.center projectile
  velocityContext.moveTo(float c.X, float c.Y)
  velocityContext.lineTo(
    float c.X + float projectile.Velocity.X,
    float c.Y + float projectile.Velocity.Y)
  velocityContext.closePath()
  velocityContext.stroke()



let redrawProjectile prev projectile =
  clearBody prev
  clearVelocityMarker prev
  drawBody projectile
  drawVelocityMarker projectile
