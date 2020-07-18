module App

open System
open FSharp.Data.UnitSystems.SI.UnitSymbols
open Elmish
open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props

importAll "../sass/main.sass"

//----Model----

// Not sure if Vector2 supports units of measure. It also has data of type float32 (less accurate than float).
// So here we make our own:
type Vector2D<[<Measure>]'u> =
  { X: float<'u>
    Y: float<'u> }
  // currently a problem with the fable complier,
  // as overloaded operators don't always work: https://github.com/fable-compiler/Fable/issues/2101
  // So here we have to use functions:
  member this.Add vector  =
    { X = this.X + vector.X
      Y = this.Y + vector.Y }
  member this.Subtract vector =
    { X = this.X - vector.X
      Y = this.Y - vector.Y }
  member this.Multiply value =
    { X = this.X * value
      Y = this.Y * value }
  member this.Dot vector =
    vector.X * this.X + vector.Y * this.Y
  // For when it gets fixed:
  //static member (*) (vector: Vector2D<'u>, value: float<'u2>) =
  //  { X = vector.X * value
  //    Y = vector.Y * value }
  //static member (*) (value: float<'u2>, vector: Vector2D<'u>) =
  //  vector * value
  //static member (+) (vector, other) =
  //  { X = vector.X + other.X
  //    Y = vector.Y + other.Y }
  //static member (-) (vector, other) =
  //  { X = vector.X - other.X
  //    Y = vector.Y - other.Y }

type Body =
  { Width: int<m>
    Height: int<m>
    Mass: float<kg>
    Position: Vector2D<m>
    Velocity: Vector2D<m/s>
    Acceleration: Vector2D<m/s^2>
    PrevAcceleration: Vector2D<m/s^2> }
  member this.Center =
    { X = this.Position.X + (float this.Width) * 0.5<m>
      Y = this.Position.Y + (float this.Height) * 0.5<m> }

[<Measure>] type deg

let magnitudeDegreesToVector (magnitude: float<'u>) (degrees: float<deg>) =
  { X = magnitude * cos(degrees * Math.PI / 180.0<deg>)
    Y = magnitude * sin(degrees * Math.PI / 180.0<deg>) }

let normalize (vector: Vector2D<'u>) =
  let magnitude = sqrt (vector.Dot vector)
  { X = vector.X / magnitude
    Y = vector.Y / magnitude }

// Force drag = [direction vector] * [various constants = c] * v^2
let airFriction (velocity: Vector2D<m/s>) (constant: float<kg/m>) =
  (normalize velocity).Multiply (-constant * (velocity.Dot velocity))

let apply gravity acceleration : Vector2D<m/s^2> =
  { acceleration with Y = acceleration.Y + gravity }

let calcAcceleration gravity airFrictionConstant (mass: float<kg>) velocity =
  (airFriction velocity airFrictionConstant).Multiply (1.0 / mass) // divide by mass to get acceleration
  |> apply gravity // add acceleration due to gravity (not force gravity)

type Model =
  { Width: int<m>
    Height: int<m>
    Projectile: Body
    InitialSpeed: float<m/s>
    InitialAngle: float<deg>
    InitialPosition: Vector2D<m>
    AccelerationDueToGravity: float<m/s^2>
    AirFrictionConstant: float<kg/m>
    ShowTrajectory: bool
    ShowVelocityMarker: bool 
    SecondsPerTrajectoryTracer: float<s>
    LastTrajectoryTracer: float<s>
    Started: bool
    Running: bool
    SimulationSpeed: float
    JumpStep: float<s>
    TimeStep: float<s>
    Time: float<s>
    LeftOverTime: float<s> }
  member this.Acceleration body =
    calcAcceleration this.AccelerationDueToGravity this.AirFrictionConstant body.Mass body.Velocity

let init () =
  let height = 700<m>
  let projectileLength = 25<m>
  let mass = 100.0<kg>
  let initialSpeed = 120.0<m/s>
  let initialAngle = 70.0<deg>
  let position = { X = 15.0<m>; Y = 15.0<m> }
  let velocity = magnitudeDegreesToVector initialSpeed initialAngle
  let accelerationDueToGravity = -10.0<m/s^2>
  let airFrictionConstant = 0.0<kg/m>
  let acceleration = calcAcceleration accelerationDueToGravity airFrictionConstant mass velocity
  let projectile =
    { Width = projectileLength
      Height = projectileLength
      Mass = mass
      Position = position
      Velocity = velocity
      Acceleration = acceleration
      PrevAcceleration = acceleration }
  { Width = 1000<m>
    Height = height
    Projectile = projectile
    InitialSpeed = initialSpeed
    InitialAngle = initialAngle
    InitialPosition = position
    AccelerationDueToGravity = accelerationDueToGravity
    AirFrictionConstant = airFrictionConstant
    ShowTrajectory = true
    ShowVelocityMarker = true 
    SecondsPerTrajectoryTracer = 1.0<s>
    LastTrajectoryTracer = 0.0<s>
    Started = false
    Running = false
    SimulationSpeed = 5.0
    JumpStep = 1.0<s>
    TimeStep = 1.0<s>/120.0
    Time = 0.0<s>
    LeftOverTime = 0.0<s> }

let initialModel = init ()

open Browser.Dom
open Browser.Types

// Initialize canvases
let container = document.getElementById("canvases") :?> HTMLDivElement
container.setAttribute(
  "style",
  sprintf
    "border: solid 1px black;
     margin: 5px;
     flex-shrink: 0;
     width:%ipx;
     height:%ipx"
    initialModel.Width
    initialModel.Height)

let setCanvasSize id =
  let canvas = document.getElementById(id) :?> HTMLCanvasElement
  canvas.width <- float initialModel.Width
  canvas.height <- float initialModel.Height
  canvas, canvas.getContext_2d()

let trajectoryCanvas, trajectoryContext = setCanvasSize "trajectoryCanvas"
let bodyCanvas, bodyContext = setCanvasSize "bodyCanvas"
let velocityMarkerCanvas, velocityMarkerContext = setCanvasSize "velocityMarkerCanvas"

trajectoryContext.fillStyle <- U3.Case1 "black"
bodyContext.fillStyle <- U3.Case1 "blue"
velocityMarkerContext.strokeStyle <- U3.Case1 "red"

// HTMLCanvas uses top-left corner as (0,0) with positive y meaning downwards on the screen.
// Translate the canvas so that (0,0) is at least the bottom left corner (still have to multiply y-coordinate by -1 when drawing):
let translateToCartesian (context: CanvasRenderingContext2D) =
  context.translate(0.0, context.canvas.height)

translateToCartesian bodyContext
translateToCartesian velocityMarkerContext
translateToCartesian trajectoryContext

// Draw functions:
let clearVelocityMarker (projectile: Body) =
  velocityMarkerContext.clearRect(
    float projectile.Center.X - float (abs projectile.Velocity.X) - 1.0,
    -float projectile.Center.Y - float (abs projectile.Velocity.Y) - 1.0,
    float (2.0 * (abs projectile.Velocity.X)) + 2.0,
    float (2.0 * (abs projectile.Velocity.Y)) + 2.0)

let drawVelocityMarker (projectile: Body) =
  velocityMarkerContext.beginPath()
  velocityMarkerContext.moveTo(float projectile.Center.X, -float projectile.Center.Y)
  velocityMarkerContext.lineTo(
    float projectile.Center.X + float projectile.Velocity.X,
    -float projectile.Center.Y - float projectile.Velocity.Y)
  velocityMarkerContext.closePath()
  velocityMarkerContext.stroke()

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

let drawTrajectory (projectile: Body) =
  trajectoryContext.beginPath()
  trajectoryContext.arc(
    float projectile.Center.X,
    -float projectile.Center.Y,
    2.5,
    0.0,
    Math.PI * 2.0)
  trajectoryContext.closePath()
  trajectoryContext.fill()

let clear (context: CanvasRenderingContext2D) =
  context.clearRect(0.0, -context.canvas.height, context.canvas.width, context.canvas.height)

// Draw initial state of projectile
drawBody initialModel.Projectile
drawVelocityMarker initialModel.Projectile

// ----Update----
type Message =
  | SetInitialSpeed of float
  | SetInitialAngle of float
  | SetInitialX of float
  | SetInitialY of float
  | SetMass of float
  | SetAccelerationDueToGravity of float
  | SetAirFrictionConstant of float
  | ToggleShowTrajectory
  | ToggleShowVelocityMarker
  | SetSecondsPerTrajectoryTracer of float
  | SetJumpStep of float
  | SetSimulationSpeed of float
  | StartStop
  | NextFrame of float
  | Jump
  | Reset

let rec loop dispatch last t =
  dispatch (NextFrame (t - last))
  window.requestAnimationFrame((loop dispatch t)) |> ignore

// Let program subscribe to frame updates from the browser
let animiate _ = Cmd.ofSub (fun d -> (loop d 0.0 0.0)) 

let move body (time: float<s>) (newAcceleration: Vector2D<m/s/s>) =
  // This looks so terrible without operators...
  { body with
      // body.Position + body.Velocity * time + (1.0 / 6.0) * (4.0 * body.Acceleration - prevAcceleration) * time * time
      Position =
        (body.Position
          .Add (body.Velocity.Multiply time))
          .Add (((body.Acceleration.Multiply 4.0).Subtract body.PrevAcceleration).Multiply (time * time / 6.0))
      // body.Velocity + (1.0 / 6.0) * (2.0 * newAcceleration + 5.0 * body.Acceleration - prevAcceleration) * time
      Velocity =
        body.Velocity.Add
          ((((newAcceleration.Multiply 2.0)
              .Add (body.Acceleration.Multiply 5.0))
              .Subtract body.PrevAcceleration)
            .Multiply (time / 6.0))
      Acceleration = newAcceleration
      PrevAcceleration = body.Acceleration }

let simulate model totalTime =
  clearBody model.Projectile
  clearVelocityMarker model.Projectile
  if (not model.Started) then
    drawTrajectory model.Projectile

  let numSteps = int (floor (totalTime / model.TimeStep))
  let mutable lastTrajectoryTracer = model.LastTrajectoryTracer
  let mutable projectile = model.Projectile
  let mutable newProjectile = model.Projectile
  for step = 1 to numSteps do
    newProjectile <- move projectile model.TimeStep (model.Acceleration projectile)
    
    let timeSinceLastTracer = model.Time + model.TimeStep * float step - lastTrajectoryTracer
    if timeSinceLastTracer >= model.SecondsPerTrajectoryTracer then
      let timeAfterTracer = timeSinceLastTracer - model.SecondsPerTrajectoryTracer
      // alpha: how far into the timeStep should the tracer be drawn
      let alpha = (model.TimeStep - timeAfterTracer) / model.TimeStep
      // Blend / perform a linear interpolation between the projectile at the start and at the end of the time step based on alpha
      // (1.0 - alpha) * projectile.Position + alpha * newProjectile.Position
      drawTrajectory { projectile with Position = (projectile.Position.Multiply (1.0 - alpha)).Add (newProjectile.Position.Multiply alpha) }
      lastTrajectoryTracer <- lastTrajectoryTracer + model.SecondsPerTrajectoryTracer

    projectile <- newProjectile

  let leftOverTime = totalTime - float numSteps * model.TimeStep
  let newTime = model.Time + totalTime - leftOverTime
  drawBody projectile
  drawVelocityMarker projectile
  { model with
      Started = true
      Time = newTime
      LeftOverTime = leftOverTime
      LastTrajectoryTracer = newTime - ((newTime - model.LastTrajectoryTracer) % model.SecondsPerTrajectoryTracer)
      Projectile = projectile }

let update message model =
  match message with
  | SetInitialSpeed v ->
      clearVelocityMarker model.Projectile
      let speed = v * 1.0<m/s>
      let projectile = { model.Projectile with Velocity = magnitudeDegreesToVector speed model.InitialAngle }
      drawVelocityMarker projectile
      { model with
          InitialSpeed = speed
          Projectile = projectile }
  | SetInitialAngle a ->
      clearVelocityMarker model.Projectile
      let angle = a * 1.0<deg>
      let projectile = { model.Projectile with Velocity = magnitudeDegreesToVector model.InitialSpeed angle }
      drawVelocityMarker projectile
      { model with
          InitialAngle = angle
          Projectile = projectile }
  | SetInitialX x ->
      clearBody model.Projectile
      clearVelocityMarker model.Projectile
      let position = { model.InitialPosition with X = x * 1.0<m> }
      let projectile = { model.Projectile with Position = position }
      drawBody projectile
      drawVelocityMarker projectile
      { model with
          Projectile = projectile
          InitialPosition = position }
  | SetInitialY y ->
      clearBody model.Projectile
      clearVelocityMarker model.Projectile
      let position = { model.InitialPosition with Y = y * 1.0<m> }
      let projectile = { model.Projectile with Position = position }
      drawBody projectile
      drawVelocityMarker projectile
      { model with
          Projectile = projectile
          InitialPosition = position }
  | SetMass m ->
      let mass = (max 0.0 m) * 1.0<kg>
      if model.Started then
        { model with Projectile = { model.Projectile with Mass = mass } }
      else
        let acceleration = calcAcceleration model.AccelerationDueToGravity model.AirFrictionConstant mass model.Projectile.Velocity
        { model with
            Projectile = 
              { model.Projectile with
                  Mass = mass
                  Acceleration = acceleration
                  PrevAcceleration = acceleration } }
  | SetAccelerationDueToGravity a ->
      let accelerationDueToGravity = a * 1.0<m/s^2>
      if model.Started then
        { model with AccelerationDueToGravity = accelerationDueToGravity }
      else
        let acceleration = calcAcceleration accelerationDueToGravity model.AirFrictionConstant model.Projectile.Mass model.Projectile.Velocity
        { model with
            AccelerationDueToGravity = accelerationDueToGravity
            Projectile =
              { model.Projectile with
                  Acceleration = acceleration
                  PrevAcceleration = acceleration } }
  | SetAirFrictionConstant c ->
      let airFrictionConstant = (max 0.0 c) * 1.0<kg/m>
      if model.Started then
        { model with AirFrictionConstant = airFrictionConstant }
      else
        let acceleration = calcAcceleration model.AccelerationDueToGravity airFrictionConstant model.Projectile.Mass model.Projectile.Velocity
        { model with
            AirFrictionConstant = airFrictionConstant
            Projectile =
              { model.Projectile with
                  Acceleration = acceleration
                  PrevAcceleration = acceleration } }
  | ToggleShowTrajectory ->
      trajectoryCanvas.hidden <- model.ShowTrajectory
      { model with ShowTrajectory = not model.ShowTrajectory }
  | ToggleShowVelocityMarker ->
      velocityMarkerCanvas.hidden <- model.ShowVelocityMarker
      { model with ShowVelocityMarker = not model.ShowVelocityMarker }
  | SetSecondsPerTrajectoryTracer s -> { model with SecondsPerTrajectoryTracer = max model.TimeStep (s * 1.0<s>) }
  | SetJumpStep j -> { model with JumpStep = max model.TimeStep (j * 1.0<s>) }
  | SetSimulationSpeed x -> { model with SimulationSpeed = max 0.0 x }
  | StartStop ->
      if model.Running then
        { model with
            Running = false
            LeftOverTime = 0.0<s> }
      else
        { model with Running = true }
  | NextFrame t ->
      if model.Running then
        simulate model ((min (t * 0.001<s>) 0.1<s>) * model.SimulationSpeed + model.LeftOverTime)
      else model
  | Jump -> simulate model model.JumpStep
  | Reset ->
      clearBody model.Projectile
      clearVelocityMarker model.Projectile
      clear trajectoryContext
      let projectile =
        let velocity = magnitudeDegreesToVector model.InitialSpeed model.InitialAngle
        let acceleration = calcAcceleration model.AccelerationDueToGravity model.AirFrictionConstant model.Projectile.Mass velocity
        { model.Projectile with
            Position = model.InitialPosition
            Velocity = velocity
            Acceleration = acceleration
            PrevAcceleration = acceleration }
      drawBody projectile
      drawVelocityMarker projectile
      { model with
          Projectile = projectile
          Time = 0.0<s>
          LeftOverTime = 0.0<s>
          LastTrajectoryTracer = 0.0<s>
          Started = false
          Running = false }

// ----View----
open Elmish.React

let viewButton message text disabled dispatch =
  li []
    [ button
        [ Type "button"
          ClassName "button"
          OnClick (fun _ -> dispatch message)
          Disabled disabled ]
        [ str text ] ]

let startStop running dispatch =
  li []
    [ button
        [ Type "button"
          ClassName "button"
          OnClick (fun _ -> dispatch StartStop) ]
        [ str (if running then "Stop" else "Start") ] ]

let viewCheckbox text message isChecked dispatch =
  li []
    [ label []
        [ input
            [ Type "checkbox"
              Checked isChecked
              OnChange (fun _ -> dispatch message) ]
          str text ] ]

let viewSetting attributes name unitName message value dispatch =
  li [ ClassName "setting" ]
    [ label []
        [ str (name + ": ")
          input
            ( attributes @
              [ Type "number"
                ClassName "number-input"
                valueOrDefault value
                OnChange (fun e -> dispatch <| message !!e.Value) ] )
          str unitName ] ]

let viewInitialSetting name unitName message value started dispatch =
  viewSetting [ Disabled started ] ("Initial " + name) unitName message value dispatch 

let viewTableRow propertyName value =
  tr []
    [ td [] [ str (propertyName + ":") ]
      td [] [ str (sprintf "%.5f" value) ] ]

// Projectile has no thrust, so air friction slows it down fast.
// Scale the step on the friction constant input using a magic number:
let frictionScale = 2500.0

let view model dispatch =
  div [ ClassName "settings" ]
    [ ul [ ClassName "settings-section" ]
        [ lazyView2 startStop model.Running dispatch
          lazyView2 (viewButton Reset "Reset") (not model.Started) dispatch
          viewSetting [ Min 0; Step 0.25; Disabled model.Running ] "Jump" "s" SetJumpStep model.JumpStep dispatch
          lazyView2 (viewButton Jump "Jump") model.Running dispatch]
      ul [ ClassName "settings-section" ]
        [ lazyView2 (viewCheckbox "Show Velocity Marker" ToggleShowVelocityMarker) model.ShowVelocityMarker dispatch
          lazyView2 (viewCheckbox "Show Trajectory" ToggleShowTrajectory) model.ShowTrajectory dispatch
          li []
            [ table []
                [ tbody []
                    [ lazyView (viewTableRow "X") (float model.Projectile.Position.X)
                      lazyView (viewTableRow "Y") (float model.Projectile.Position.Y)
                      lazyView (viewTableRow "VX") (float model.Projectile.Velocity.X)
                      lazyView (viewTableRow "VY") (float model.Projectile.Velocity.Y)
                      lazyView (viewTableRow "AX") (float model.Projectile.Acceleration.X)
                      lazyView (viewTableRow "AY") (float model.Projectile.Acceleration.Y)
                      lazyView (viewTableRow "Time") (float model.Time) ] ] ] ]
      ul [ ClassName "settings-section" ]
        [ lazyView2 (viewSetting [ Min 0; Step 0.25 ] "Simulation Speed" "x" SetSimulationSpeed) model.SimulationSpeed dispatch
          lazyView2 (viewSetting [ Min 0; Step 0.25 ] "Trace Trajectory Every" "s" SetSecondsPerTrajectoryTracer) model.SecondsPerTrajectoryTracer dispatch //min
          lazyView2 (viewSetting [ Min 0 ] "Mass" "kg" SetMass) model.Projectile.Mass dispatch
          lazyView2 (viewSetting [] "Acceleration Due to Gravity" "m/s^2" SetAccelerationDueToGravity) model.AccelerationDueToGravity dispatch
          lazyView2 (viewSetting [ Min 0; Step (model.Projectile.Mass / frictionScale) ] "Air Friction Constant" "kg/m" SetAirFrictionConstant) model.AirFrictionConstant dispatch ] //step
      ul [ ClassName "settings-section" ]
        [ lazyView3 (viewInitialSetting "Speed" "m/s" SetInitialSpeed) model.InitialSpeed model.Started dispatch
          lazyView3 (viewInitialSetting "Angle" "deg" SetInitialAngle) model.InitialAngle model.Started dispatch
          lazyView3 (viewInitialSetting "X" "m" SetInitialX) model.InitialPosition.X model.Started dispatch
          lazyView3 (viewInitialSetting "Y" "m" SetInitialY) model.InitialPosition.Y model.Started dispatch ] ]

// ----App----
open Elmish.Debug
open Elmish.HMR

Program.mkSimple (fun _ -> initialModel) update view
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withReactBatched "elmish-app"
|> Program.withSubscription animiate
|> Program.run
