module ProjectilePhysicsSimulation.App

open FSharp.Data.UnitSystems.SI.UnitSymbols
open Physics
open Graphics

open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Elmish
open Browser.Dom

importAll "../sass/main.sass"

let width = 1000
let height = 600

let init () =
  let projectileLength = 25<m>
  let mass = 100.0<kg>
  let initialSpeed = 105.0<m/s>
  let initialAngle = 60.0<deg>
  let position =
    { X = 15.0<m>
      Y = 15.0<m> }
  let velocity = magnitudeDegreesToVector initialSpeed initialAngle
  let accelerationGravity = -9.8<m/s^2>
  let dragConstant = 0.0<kg/m>
  let acceleration = calcAcceleration accelerationGravity dragConstant velocity mass
  let projectile =
    { Width = projectileLength
      Height = projectileLength
      Mass = mass
      Position = position
      Velocity = velocity
      Acceleration = acceleration
      PrevAcceleration = zero2D }

  let model =
    { Projectile = projectile
      InitialSpeed = initialSpeed
      InitialAngle = initialAngle
      InitialPosition = position
      AccelerationGravity = accelerationGravity
      DragConstant = dragConstant
      ShowTrajectory = true
      ShowVelocityMarker = true
      TraceInterval = 1.0<_>
      LastTracer = 0.0<_>
      Started = false
      Running = false
      SimulationSpeed = 2.5
      JumpStep = 1.0<_>
      Time = 0.0<_>
      LeftOverTime = 0.0<_> }

  // Initialize canvases
  let initCanvas = initCanvasWith width height
  
  initCanvas trajectoryCanvas trajectoryContext
  initCanvas bodyCanvas bodyContext
  initCanvas velocityCanvas velocityContext

  trajectoryContext.fillStyle <- U3.Case1 "black"
  bodyContext.fillStyle <- U3.Case1 "blue"
  velocityContext.strokeStyle <- U3.Case1 "red"

  // Draw initial state of projectile
  drawBody projectile
  drawVelocityMarker projectile

  model

// ----Update----
type Message =
  | SetInitialSpeed of float<m/s>
  | SetInitialAngle of float<deg>
  | SetInitialX of float<m>
  | SetInitialY of float<m>
  | SetMass of float<kg>
  | SetAccelerationGravity of float<m/s^2>
  | SetDragConstant of float<kg/m>
  | ToggleShowTrajectory
  | ToggleShowVelocity
  | SetTraceInterval of float<s>
  | SetJumpStep of float<s>
  | SetSimulationSpeed of float
  | StartStop
  | NextFrame of float<ms>
  | Jump
  | Reset

let rec loop dispatch last t =
  dispatch <| NextFrame ((t - last) * 1.0<ms>)
  window.requestAnimationFrame((loop dispatch t)) |> ignore

// Let program subscribe to frame updates from the browser
let animiate _ = Cmd.ofSub <| fun d -> loop d 0.0 0.0

type Float = float

let update message model =
  match message with
  | SetInitialSpeed v ->
      clearVelocityMarker model.Projectile
      let projectile = { model.Projectile with Velocity = magnitudeDegreesToVector v model.InitialAngle }
      drawVelocityMarker projectile
      { model with
          InitialSpeed = v
          Projectile = projectile }
  | SetInitialAngle a ->
      clearVelocityMarker model.Projectile
      let projectile = { model.Projectile with Velocity = magnitudeDegreesToVector model.InitialSpeed a }
      drawVelocityMarker projectile
      { model with
          InitialAngle = a
          Projectile = projectile }
  | SetInitialX x ->
      clearBody model.Projectile
      clearVelocityMarker model.Projectile
      let position = { model.InitialPosition with X = x }
      let projectile = { model.Projectile with Position = position }
      drawBody projectile
      drawVelocityMarker projectile
      { model with
          Projectile = projectile
          InitialPosition = position }
  | SetInitialY y ->
      clearBody model.Projectile
      clearVelocityMarker model.Projectile
      let position = { model.InitialPosition with Y = y }
      let projectile = { model.Projectile with Position = position }
      drawBody projectile
      drawVelocityMarker projectile
      { model with
          Projectile = projectile
          InitialPosition = position }
  | SetMass m ->
      let mass = max (Float.Epsilon * 1.0<_>) m
      if model.Started then
        { model with Projectile = { model.Projectile with Mass = mass } }
      else
        let acceleration = calcAccelerationUsing model model.Projectile.Velocity mass
        { model with
            Projectile =
              { model.Projectile with
                  Mass = mass
                  Acceleration = acceleration } }
  | SetAccelerationGravity a ->
      if model.Started then
        { model with AccelerationGravity = a }
      else
        { model with
            AccelerationGravity = a
            Projectile = { model.Projectile with Acceleration = calcBodyAcceleration a model.DragConstant model.Projectile } }
  | SetDragConstant c ->
      let airFrictionConstant = max 0.0<_> c
      if model.Started then
        { model with DragConstant = airFrictionConstant }
      else
        { model with
            DragConstant = airFrictionConstant
            Projectile = { model.Projectile with Acceleration = calcBodyAcceleration model.AccelerationGravity airFrictionConstant model.Projectile } }
  | ToggleShowTrajectory ->
      trajectoryCanvas.hidden <- model.ShowTrajectory
      { model with ShowTrajectory = not model.ShowTrajectory }
  | ToggleShowVelocity ->
      velocityCanvas.hidden <- model.ShowVelocityMarker
      { model with ShowVelocityMarker = not model.ShowVelocityMarker }
  | SetTraceInterval s -> { model with TraceInterval = max timeStep s }
  | SetJumpStep j -> { model with JumpStep = max timeStep j }
  | SetSimulationSpeed x -> { model with SimulationSpeed = max 0.0 x }
  | StartStop ->
      if model.Running then
        { model with
            Running = false
            LeftOverTime = 0.0<_> }
      else
        { model with Running = true }
  | NextFrame t ->
      if model.Running
      then simulateAndDraw model ((min 0.1<_> (t / 1000.0<ms/s>)) * model.SimulationSpeed + model.LeftOverTime)
      else model
  | Jump -> simulateAndDraw model model.JumpStep
  | Reset ->
      clearBody model.Projectile
      clearVelocityMarker model.Projectile
      clear trajectoryContext
      let projectile =
        let velocity = magnitudeDegreesToVector model.InitialSpeed model.InitialAngle
        let acceleration = calcAccelerationUsing model velocity model.Projectile.Mass
        { model.Projectile with
            Position = model.InitialPosition
            Velocity = velocity
            Acceleration = acceleration
            PrevAcceleration = zero2D }
      drawBody projectile
      drawVelocityMarker projectile
      { model with
          Projectile = projectile
          Time = 0.0<_>
          LeftOverTime = 0.0<_>
          LastTracer = 0.0<_>
          Started = false
          Running = false }

// ----View----
open Elmish.React

let withSub subscript text = [ str text; sub [] [ str subscript ] ]

let withSuper superscript text = [ str text; sup [] [ str superscript ] ]

let checkbox text message isChecked dispatch =
  li []
    [ label [ ClassName "checkbox" ]
        [ input
            [ Type "checkbox"
              Checked isChecked
              OnChange <| fun _ -> dispatch message ]
          str text ] ]

let inline numberInputUnit attributes unit message value dispatch =
  div []
    [ input
        [ Type "number"
          valueOrDefault value
          OnChange <| fun e -> dispatch <| message (float e.Value * 1.0<_>)
          yield! attributes ]
      yield! unit ]

let inline numberInput attributes unitName = numberInputUnit attributes [ str unitName ]

let inline viewSettingUnit attributes name unit message value dispatch =
  li []
    [ label []
        [ span []
            [ str <| name + ":" ]
          numberInputUnit attributes unit message value dispatch ] ]

let inline viewSetting attributes name unitName = viewSettingUnit attributes name ([ str unitName ])

let inline viewInitialSetting name unitName message value started dispatch =
  viewSetting [ Disabled started ] ("Initial " + name) unitName message value dispatch 

let inline telemetryEntryWith name value =
  tr []
    [ td []
        [ yield! name
          str ":" ]
      td []
        [ str <| sprintf "%.3f" (float value) ] ]

let inline telemetryEntry name = telemetryEntryWith [ str name ]

let inline telemetryEntrySub name sub = telemetryEntryWith (name |> withSub sub)

// Projectile has no thrust, so air friction slows it down fast.
// Scale the step on the friction constant input using a magic number:
let frictionScale = 2500.0

let view model dispatch =
  ofList
    [ div
        [ ClassName "simulation"
          Key "simulation"
          Style [ Width width ] ]
        [ div
            [ ClassName "controls"
              Style [ MarginTop height ] ]
            [ button
                [ Type "button"
                  classBaseList
                    "playpause"
                    [ "play", not model.Running
                      "pause", model.Running ]
                  Title <| if model.Running then "Pause" else "Play"
                  OnClick <| fun _ -> dispatch StartStop ]
                []
              label []
                [ str "Speed:"
                  input
                    [ Type "number"
                      ClassName "number-input"
                      Min 0
                      Step 0.25
                      valueOrDefault model.SimulationSpeed
                      OnChange <| fun e -> dispatch <| SetSimulationSpeed (float e.Value) ]
                  str "x" ]
              div []
                [ button
                    [ Type "button"
                      ClassName "jump"
                      Title "Jump"
                      OnClick <| fun _ -> dispatch Jump ]
                    []
                  lazyView2
                    (numberInput
                      [ Style
                          [ Width "3em"
                            MarginLeft 0
                            MarginTop "0.1em" ]
                        Min 0
                        Step 0.25 ]
                      "s"
                      SetJumpStep)
                    model.JumpStep
                    dispatch ]
              button
                [ Type "button"
                  ClassName "reset"
                  Title "Reset"
                  OnClick <| fun _ -> dispatch Reset ]
                [] ]
          details
            [ ClassName "telemetry"
              Open true ]
            [ summary []
                [ str "Telemetry" ]
              table []
                [ tbody []
                    [ lazyView (telemetryEntry "T") model.Time
                      lazyView (telemetryEntry "X") model.Projectile.Position.X
                      lazyView (telemetryEntry "Y") model.Projectile.Position.Y
                      lazyView (telemetryEntrySub "V" "x") model.Projectile.Velocity.X
                      lazyView (telemetryEntrySub "V" "y") model.Projectile.Velocity.Y
                      lazyView (telemetryEntrySub "A" "x") model.Projectile.Acceleration.X
                      lazyView (telemetryEntrySub "A" "y") model.Projectile.Acceleration.Y ] ] ] ]
      div
        [ ClassName "settings"
          Key "settings" ]
        [ ul []
            [ lazyView2 (checkbox "Show Velocity Marker" ToggleShowVelocity) model.ShowVelocityMarker dispatch
              lazyView2 (checkbox "Show Trajectory" ToggleShowTrajectory) model.ShowTrajectory dispatch
              lazyView2 (viewSetting [ Min 0; Step 0.25 ] "Trace Trajectory Every" "s" SetTraceInterval) (if model.TraceInterval = timeStep then 0.0<_> else model.TraceInterval) dispatch ]
          ul []
            [ lazyView2 (viewSetting [ Min 0; ] "Mass" "kg" SetMass) (if float model.Projectile.Mass = Float.Epsilon then 0.0<_> else model.Projectile.Mass) dispatch
              lazyView2 (viewSettingUnit [] "Gravity" ("m/s" |> withSuper "2") SetAccelerationGravity) model.AccelerationGravity dispatch
              li []
                [ label []
                    [ span [ Title "Combined Drag Constant = fluid density (kg/m^3) * contact area (m^2) * drag coefficient (unitless) / 2" ]
                        [ u [ ClassName "tooltip" ]
                            [ str "Combined Drag Constant" ]
                          str ":" ]
                      lazyView2
                        (numberInput
                          [ Min 0
                            Step (model.Projectile.Mass / frictionScale) ]
                          "kg/m"
                          SetDragConstant)
                        model.DragConstant
                        dispatch ] ] ]
          ul []
            [ lazyView3 (viewInitialSetting "X" "m" SetInitialX) model.InitialPosition.X model.Started dispatch
              lazyView3 (viewInitialSetting "Y" "m" SetInitialY) model.InitialPosition.Y model.Started dispatch
              lazyView3 (viewInitialSetting "Speed" "m/s" SetInitialSpeed) model.InitialSpeed model.Started dispatch
              lazyView3 (viewInitialSetting "Angle" "deg" SetInitialAngle) model.InitialAngle model.Started dispatch ] ] ]

// ----App----
open Elmish.Debug
open Elmish.HMR

Program.mkSimple init update view
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withReactBatched "elmish-app"
|> Program.withSubscription animiate
|> Program.run