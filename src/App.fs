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


let inline clamp low high = max low >> min high

let simulateAndDraw sim totalTime =
  let sim = Simulation.simulate (Graphics.maybeDrawTracer sim) sim totalTime
  Graphics.redrawBody sim.Projectile
  Graphics.redrawVelocityMarker sim.Projectile
  sim

let newPosition settings sim =
  let sim = sim |> Simulation.withSettings settings
  Graphics.redrawAll sim.Projectile
  sim

let newVelocity settings sim =
  let sim = sim |> Simulation.withSettings settings
  Graphics.redrawVelocityMarker sim.Projectile
  sim

let newAcceleration settings sim =
  if sim.Running
  then { sim with Settings = settings }
  else sim |> Simulation.withSettings settings

let update message sim =
  match message with
  | SetInitialSpeed v -> sim |> newVelocity { sim.Settings with InitialSpeed = v }

  | SetInitialAngle a -> sim |> newVelocity { sim.Settings with InitialAngle = a }

  | SetInitialX x -> sim |> newPosition { sim.Settings with InitialPosition = sim.Settings.InitialPosition |> Vector2.withX x }
          
  | SetInitialY y -> sim |> newPosition { sim.Settings with InitialPosition = sim.Settings.InitialPosition |> Vector2.withY y }

  | SetMass m ->
      let projectile = { sim.Projectile with Mass = max Body.minMass m }
      if sim.Running
      then { sim with Projectile = projectile }
      else { sim with Projectile = Simulation.initialProjectile sim.Settings projectile }

  | SetAccelerationGravity a -> sim |> newAcceleration { sim.Settings with AccelerationGravity = a }

  | SetDragConstant c -> sim |> newAcceleration { sim.Settings with DragConstant = max Simulation.minDragConstant c }

  | ToggleShowTrajectory ->
      Graphics.trajectoryCanvas.hidden <- sim.Settings.ShowTrajectory
      { sim with Settings = { sim.Settings with ShowTrajectory = not sim.Settings.ShowTrajectory } }

  | ToggleShowVelocity ->
      Graphics.velocityCanvas.hidden <- sim.Settings.ShowVelocityMarker
      { sim with Settings = { sim.Settings with ShowVelocityMarker = not sim.Settings.ShowVelocityMarker } }

  | SetTraceInterval s ->
      let trace = max Simulation.minTraceInterval s
      if trace <> sim.Settings.TraceInterval then
        let lastTracer =
          if sim.Time - sim.LastTracer >= sim.Settings.TraceInterval then
            Graphics.drawTracer <| Body.center sim.Projectile
            sim.Time
          else
            sim.LastTracer
        { sim with
            LastTracer = lastTracer
            Settings = { sim.Settings with TraceInterval = trace } }
      else
        sim

  | SetJumpStep j ->
      { sim with Settings = { sim.Settings with JumpStep = j |> clamp Simulation.minJumpStep Simulation.maxJumpStep } }

  | SetSimulationSpeed x ->
      { sim with Settings = { sim.Settings with SimulationSpeed = x |> clamp Simulation.minSpeed Simulation.maxSpeed } }

  | StartStop ->
      if sim.Running then
        { sim with
            Running = false
            LeftOverTime = 0.0<_> }
      else
        { sim with Running = true }

  | NextFrame t ->
      if sim.Running && sim.Settings.SimulationSpeed > 0.0
      then simulateAndDraw sim ((min 0.1<_> (t / 1000.0<ms/s>)) * sim.Settings.SimulationSpeed + sim.LeftOverTime)
      else sim

  | Jump -> simulateAndDraw sim sim.Settings.JumpStep

  | Reset ->
      let projectile = Simulation.initialProjectile sim.Settings sim.Projectile
      Graphics.redrawAll projectile
      { sim with
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


let controls running settings dispatch =
  printf "controls"
  div
    [ ClassName "controls"
      Style [ MarginTop Graphics.height ] ]
    [ button
        [ let playpause = if running then "Pause" else "Play"
          Type "button"
          ClassName (playpause.ToLower())
          Title playpause
          OnClick <| fun _ -> dispatch StartStop ]
        []
      label []
        [ str "Speed:"
          numberInputUnitWith
            [ Min 0
              Max Simulation.maxSpeed ]
            (unit "x")
            SetSimulationSpeed
            settings.SimulationSpeed
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
              Max Simulation.maxJumpStep ]
            (unit "s")
            SetJumpStep
            settings.JumpStep
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


let viewSettings started mass settings dispatch =
  div
    [ ClassName "settings"
      Key "settings" ]
    [ ul []
        [ checkbox "Show Velocity Marker" ToggleShowVelocity settings.ShowVelocityMarker dispatch
          checkbox "Show Traced Trajectory" ToggleShowTrajectory settings.ShowTrajectory dispatch
          settingInputUnitWith
            [ Min 0 ]
            "Trace Trajectory Every"
            (unit "s")
            SetTraceInterval
            settings.TraceInterval
            dispatch ]
      ul []
        [ settingInputUnitWith
            [ Min 0 ]
            "Mass"
            (unit "kg")
            SetMass
            mass
            dispatch
          settingInputUnit
            "Gravity"
            ("m/s" |> withSuper "2")
            SetAccelerationGravity
            settings.AccelerationGravity
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
                    settings.DragConstant
                    dispatch ] ] ]
      ul []
        [ let inline initSetting name u message value = settingInputUnitWith [ Disabled started ] ("Initial " + name) (unit u) message value dispatch
          initSetting "X" "m" SetInitialX settings.InitialPosition.X
          initSetting "Y" "m" SetInitialY settings.InitialPosition.Y
          initSetting "Speed" "m/s" SetInitialSpeed settings.InitialSpeed
          initSetting "Angle" "deg" SetInitialAngle settings.InitialAngle ] ]


let inline structalThenReferenceEquality (a1, b1: 'b) (a2, b2: 'b) =
  a1 = a2 && System.Object.ReferenceEquals(b1, b2)

let view sim dispatch =
  ofList
    [ div
        [ ClassName "simulation"
          Key "simulation"
          Style [ Width Graphics.width ] ]
        [ lazyView3With
            structalThenReferenceEquality
            controls
            sim.Running
            sim.Settings
            dispatch
          telemetry sim.Time sim.Projectile ]
      lazyView3With
        structalThenReferenceEquality
        (fun (started, mass) settings -> viewSettings started mass settings)
        (Simulation.started sim, sim.Projectile.Mass)
        sim.Settings
        dispatch ]




// ----App----
open Elmish
#if DEBUG
open Elmish.HMR
open Elmish.Debug
#endif

let init () =
  let sim = Simulation.initial
  Graphics.redrawAll sim.Projectile
  sim

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
