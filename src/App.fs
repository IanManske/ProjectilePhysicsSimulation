module ProjectilePhysicsSimulation.App

open FSharp.Data.UnitSystems.SI.UnitSymbols
open Physics


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


let simulateAndDraw model totalTime =
  let newModel = simulate (Graphics.maybeDrawTracer model) model totalTime
  Graphics.redrawBody newModel.Projectile
  Graphics.redrawVelocityMarker newModel.Projectile
  newModel

let newAcceleration model =
  model |>
    if model.Running
    then id
    else Model.recalculateAcceleration

let inline clamp low high = max low >> min high


let update message model =
  match message with
  | SetInitialSpeed v ->
      let projectile = { model.Projectile with Velocity = Vector2.ofMagnitudeDegrees v model.InitialAngle }
      Graphics.redrawVelocityMarker projectile
      { model with
          InitialSpeed = v
          Projectile = projectile }

  | SetInitialAngle a ->
      let projectile = { model.Projectile with Velocity = Vector2.ofMagnitudeDegrees model.InitialSpeed a }
      Graphics.redrawVelocityMarker projectile
      { model with
          InitialAngle = a
          Projectile = projectile }

  | SetInitialX x ->
      let position = model.InitialPosition |> Vector2.withX x
      let projectile = { model.Projectile with Position = position }
      Graphics.redrawBody projectile
      Graphics.redrawVelocityMarker projectile
      Graphics.resetTrajectory projectile

      { model with
          Projectile = projectile
          InitialPosition = position }
          
  | SetInitialY y ->
      let position = model.InitialPosition |> Vector2.withY y
      let projectile = { model.Projectile with Position = position }
      Graphics.redrawAll projectile
      { model with
          Projectile = projectile
          InitialPosition = position }

  | SetMass m ->
      { model with Projectile = { model.Projectile with Mass = max Body.minMass m } }
      |> newAcceleration

  | SetAccelerationGravity a ->
      { model with AccelerationGravity = a }
      |> newAcceleration

  | SetDragConstant c ->
      { model with DragConstant = max Model.minDragConstant c }
      |> newAcceleration

  | ToggleShowTrajectory ->
      Graphics.trajectoryCanvas.hidden <- model.ShowTrajectory
      { model with ShowTrajectory = not model.ShowTrajectory }

  | ToggleShowVelocity ->
      Graphics.velocityCanvas.hidden <- model.ShowVelocityMarker
      { model with ShowVelocityMarker = not model.ShowVelocityMarker }

  | SetTraceInterval s ->
      let trace = max Model.minTraceInterval s
      if trace <> model.TraceInterval then
        let lastTracer =
          if model.Time - model.LastTracer >= model.TraceInterval then
            Graphics.drawTracer <| Body.center model.Projectile
            model.Time
          else
            model.LastTracer
        { model with
            TraceInterval = trace
            LastTracer = lastTracer }
      else
        model

  | SetJumpStep j -> { model with JumpStep = j |> clamp Model.minJumpStep Model.maxJumpStep }

  | SetSimulationSpeed x -> { model with SimulationSpeed = x |> clamp Model.minSpeed Model.maxSpeed }

  | StartStop ->
      if model.Running then
        { model with
            Running = false
            LeftOverTime = 0.0<_> }
      else
        { model with Running = true }

  | NextFrame t ->
      if model.Running && model.SimulationSpeed > 0.0
      then simulateAndDraw model ((min 0.1<_> (t / 1000.0<ms/s>)) * model.SimulationSpeed + model.LeftOverTime)
      else model

  | Jump -> simulateAndDraw model model.JumpStep

  | Reset ->
      let projectile =
        let velocity = Vector2.ofMagnitudeDegrees model.InitialSpeed model.InitialAngle
        let acceleration = Model.acceleration model velocity model.Projectile.Mass
        { model.Projectile with
            Position = model.InitialPosition
            Velocity = velocity
            Acceleration = acceleration
            PrevAcceleration = Vector2.zero }
      Graphics.redrawAll projectile
      { model with
          Projectile = projectile
          Time = 0.0<_>
          LeftOverTime = 0.0<_>
          LastTracer = 0.0<_>
          Running = false }



// ----View----
open Fable.React
open Fable.React.Props
open Elmish.React

let unit unitName = [ str unitName ]
let withSub subscript text = [ str text; sub [] [ str subscript ] ]
let withSuper superscript text = [ str text; sup [] [ str superscript ] ]


let checkbox text message isChecked dispatch =
  li []
    [ label [ ClassName "checkbox" ]
        [ input
            [ Type "checkbox"
              Checked isChecked
              OnChange (fun _ -> dispatch message) ]
          str text ] ]


let inline numberInputUnitWith attributes unit message value dispatch =
  div [ Style [ Display DisplayOptions.InlineBlock ] ]
    [ input
        [ Type "number"
          DefaultValue value
          Step "any"

          OnChange (fun e ->
            try (float e.Value) * 1.0<_> |> message |> dispatch
            with | _ -> ())
            
          yield! attributes ]
      yield! unit ]

let inline numberInputUnit unit = numberInputUnitWith [] unit


let inline settingInputUnitWith attributes name unit message value dispatch =
  li []
    [ label []
        [ str <| name + ":"
          numberInputUnitWith attributes unit message value dispatch ] ]

let inline settingInputUnit name = settingInputUnitWith [] name


let controls model dispatch =
  div
    [ ClassName "controls"
      Style [ MarginTop Graphics.height ] ]
    [ button
        [ let playpause = if model.Running then "Pause" else "Play"
          Type "button"
          ClassName (playpause.ToLower())
          Title playpause
          OnClick <| fun _ -> dispatch StartStop ]
        []
      label []
        [ str "Speed:"
          numberInputUnitWith
            [ Min 0
              Max Model.maxSpeed ]
            (unit "x")
            SetSimulationSpeed
            model.SimulationSpeed
            dispatch ]
      div []
        [ button
            [ Type "button"
              ClassName "jump"
              Title "Jump"
              OnClick <| fun _ -> dispatch Jump ]
            []
          numberInputUnitWith
            [ Style
                [ Width "3em"
                  MarginLeft 0
                  MarginTop "0.1em" ]
              Min 0
              Max Model.maxJumpStep ]
            (unit "s")
            SetJumpStep
            model.JumpStep
            dispatch ]
      button
        [ Type "button"
          ClassName "reset"
          Title "Reset"
          OnClick <| fun _ -> dispatch Reset ]
        [] ]


let inline telemetryEntryWith name value =
  tr []
    [ td []
        [ yield! name
          str ":" ]
      td []
        [ str <| sprintf "%.3f" (float value) ] ]

let inline telemetryEntry name = telemetryEntryWith [ str name ]

let telemetry time projectile =
  details
    [ ClassName "telemetry"
      Open true ]
    [ summary []
        [ str "Telemetry" ]
      table []
        [ tbody []
            [ telemetryEntry "T" time
              telemetryEntry "X" projectile.Position.X
              telemetryEntry "Y" projectile.Position.Y
              telemetryEntryWith ("V" |> withSub "x") projectile.Velocity.X
              telemetryEntryWith ("V" |> withSub "y") projectile.Velocity.Y
              telemetryEntryWith ("A" |> withSub "x") projectile.Acceleration.X
              telemetryEntryWith ("A" |> withSub "y") projectile.Acceleration.Y ] ] ]


let settings model dispatch =
  div
    [ ClassName "settings"
      Key "settings" ]
    [ ul []
        [ checkbox "Show Velocity Marker" ToggleShowVelocity model.ShowVelocityMarker dispatch
          checkbox "Show Traced Trajectory" ToggleShowTrajectory model.ShowTrajectory dispatch
          settingInputUnitWith
            [ Min 0 ]
            "Trace Trajectory Every"
            (unit "s")
            SetTraceInterval
            model.TraceInterval
            dispatch ]
      ul []
        [ settingInputUnitWith
            [ Min 0 ]
            "Mass"
            (unit "kg")
            SetMass
            model.Projectile.Mass
            dispatch
          settingInputUnit
            "Gravity"
            ("m/s" |> withSuper "2")
            SetAccelerationGravity
            model.AccelerationGravity
            dispatch
          li []
            [ label []
                [ span [ Title "Combined Drag Constant = fluid density (kg/m^3) * contact area (m^2) * drag coefficient (unitless) / 2" ]
                    [ u [ ClassName "tooltip" ]
                        [ str "Combined Drag Constant" ]
                      str ":" ]
                  numberInputUnitWith
                    [ Min 0 ]
                    (unit "kg/m")
                    SetDragConstant
                    model.DragConstant
                    dispatch ] ] ]
      ul []
        [ let inline initSetting name u message value = settingInputUnitWith [ Disabled (Model.started model) ] ("Initial " + name) (unit u) message value dispatch
          initSetting "X" "m" SetInitialX model.InitialPosition.X
          initSetting "Y" "m" SetInitialY model.InitialPosition.Y
          initSetting "Speed" "m/s" SetInitialSpeed model.InitialSpeed
          initSetting "Angle" "deg" SetInitialAngle model.InitialAngle ] ]


let view model dispatch =
  ofList
    [ div
        [ ClassName "simulation"
          Key "simulation"
          Style [ Width Graphics.width ] ]
        [ controls model dispatch
          telemetry model.Time model.Projectile ]
      settings model dispatch ]



// ----App----
open Elmish
#if DEBUG
open Elmish.HMR
open Elmish.Debug
#endif

let init () =
  let model = Model.initial
  Graphics.redrawAll model.Projectile
  model

let rec loop dispatch last t =
  dispatch <| NextFrame ((t - last) * 1.0<ms>)
  Browser.Dom.window.requestAnimationFrame((loop dispatch t)) |> ignore

Program.mkSimple init update view
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withReactBatched "elmish-app"
|> Program.withSubscription (fun _ -> Cmd.ofSub <| fun d -> loop d 0.0 0.0)
|> Program.run
