module ProjectilePhysicsSimulation.Physics

open FSharp.Data.UnitSystems.SI.UnitSymbols

// milisecond
[<Measure>] type ms

// degrees
[<Measure>] type deg



// Here we have some fun and make our own Vector2 type:
[<Fable.Core.Erase>]
type Vector2< [<Measure>] 'u > =
  Vector2 of
    x: float<'u> *
    y: float<'u>


// Fable3 still has trouble with overloaded operators, so we are stuck with these ones that have "."'s.
[<AutoOpen>]
module Vector2Operators =
  let inline (.+) (Vector2(ax, ay)) (Vector2(bx, by)) = Vector2(ax + bx, ay + by)

  let inline (.-) (Vector2(ax, ay)) (Vector2(bx, by)) = Vector2(ax - bx, ay - by)

  let inline (.*) (Vector2(ax, ay)) (Vector2(bx, by)) = ax * bx + ay * by

  let inline (.*..) scalar (Vector2(ax, ay)) = Vector2(scalar * ax, scalar * ay)
  let inline (..*.) (Vector2(ax, ay)) scalar = Vector2(scalar * ax, scalar * ay)

  let inline (./) (Vector2(ax, ay)) scalar = Vector2(ax / scalar, ay / scalar)


[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Vector2 =
  let zero = Vector2(0.0<_>, 0.0<_>)

  let ofMagnitudeDegrees (magnitude: float<_>) (degrees: float<deg>) =
    Vector2
      (magnitude * cos (degrees * System.Math.PI / 180.0<deg>),
       magnitude * sin (degrees * System.Math.PI / 180.0<deg>))

  let inline X (Vector2(x, _)) = x
  let inline Y (Vector2(_, y)) = y

  let inline withX x (Vector2(_, y)) = Vector2(x, y)
  let inline withY y (Vector2(x, _)) = Vector2(x, y)

  let magnitude (vector: Vector2<_>) = sqrt (vector .* vector)

  let normalizeUnsafe vector = vector ./ (magnitude vector)

  let normalize vector =
    if vector = zero
    then zero
    else normalizeUnsafe vector

type Vector2<[<Measure>]'u> with
  member this.X = Vector2.X this
  member this.Y = Vector2.Y this



type Body =
  { Width: int<m>
    Height: int<m>
    Mass: float<kg>
    Position: Vector2<m>
    Velocity: Vector2<m/s>
    Acceleration: Vector2<m/s^2>
    PrevAcceleration: Vector2<m/s^2> }

module Body =
  let minMass = System.Double.Epsilon * 1.0<kg>

  let center body =
    Vector2
      (float body.Width * 0.5<_> + body.Position.X,
       float body.Height * 0.5<_> + body.Position.Y)



type Model =
  { Projectile: Body
    InitialSpeed: float<m/s>
    InitialAngle: float<deg>
    InitialPosition: Vector2<m>

    AccelerationGravity: float<m/s^2>
    DragConstant: float<kg/m>

    ShowTrajectory: bool
    ShowVelocityMarker: bool

    TraceInterval: float<s>
    LastTracer: float<s>

    Running: bool
    SimulationSpeed: float
    JumpStep: float<s>
    Time: float<s>
    LeftOverTime: float<s> }

let timeStep = 1.0<s> / 120.0

module Model =
  let minSpeed = 0.0
  let maxSpeed = 10000.0
  let minJumpStep = float timeStep * 1.0<s>
  let maxJumpStep = 10000.0<s>
  let minDragConstant = 0.0<kg/m>
  let minTraceInterval = float (timeStep - timeStep / 1000000000.0) * 1.0<s>

  let started model = model.Time > 0.0<_>

  let private applyGravity (gravity: float<m/s^2>) (Vector2(x, y)) = Vector2(x, y + gravity)

  // Force drag = 1/2 * fluid density (kg/m^3) * contact area (m^2) * drag coefficient (unitless) * v^2 (m^2/s^2) * [direction Vector2 (unitless)]
  // https://en.wikipedia.org/wiki/Drag_%28physics%29
  // c (kg/m) = fluid density * contact area * drag coefficient / 2
  // Force drag (kg m/s^2) = c (kg/m) * v^2 (m^2/s^2) * [direction]
  let private accclerationDrag (constant: float<kg/m>) (velocity: Vector2<m/s>) mass =
    -constant * (velocity .* velocity) .*.. (Vector2.normalize velocity) ./ mass


  let private calculateAcceleration gravity dragConstant velocity mass =
    accclerationDrag dragConstant velocity mass
    |> applyGravity gravity

  let acceleration model = calculateAcceleration model.AccelerationGravity model.DragConstant
  let bodyAcceleration model body = acceleration model body.Velocity body.Mass

  let recalculateAcceleration model =
    { model with Projectile = { model.Projectile with Acceleration = bodyAcceleration model model.Projectile } }


  let initial =
    let projectileLength = 25<m>
    let mass = 100.0<kg>
    let initialSpeed = 90.0<m/s>
    let initialAngle = 60.0<deg>
    let position = Vector2(15.0<m>, 15.0<m>)
    let velocity = Vector2.ofMagnitudeDegrees initialSpeed initialAngle
    let accelerationGravity = -9.8<m/s^2>
    let dragConstant = 0.0<kg/m>
    let acceleration = calculateAcceleration accelerationGravity dragConstant velocity mass

    { Projectile =
        { Width = projectileLength
          Height = projectileLength
          Mass = mass
          Position = position
          Velocity = velocity
          Acceleration = acceleration
          PrevAcceleration = Vector2.zero }
      InitialSpeed = initialSpeed
      InitialAngle = initialAngle
      InitialPosition = position
      AccelerationGravity = accelerationGravity
      DragConstant = dragConstant
      ShowTrajectory = true
      ShowVelocityMarker = true
      TraceInterval = 1.0<_>
      LastTracer = 0.0<_>
      Running = false
      SimulationSpeed = 2.5
      JumpStep = 1.0<_>
      Time = 0.0<_>
      LeftOverTime = 0.0<_> }



// Uses Beeman's algortithm, a variation of velocity verlet (https://en.wikipedia.org/wiki/Beeman%27s_algorithm):
let private move body (time: float<s>) newAcceleration =
  let deltaPosition = body.Velocity ..*. time .+ 1.0 / 6.0 .*.. body.Acceleration ..*. (time * time)
  { body with
      Position = body.Position .+ deltaPosition
      Velocity = body.Velocity .+ 1.0 / 6.0 .*.. (2.0 .*.. newAcceleration .+ 5.0 .*.. body.Acceleration .- body.PrevAcceleration) ..*. time
      Acceleration = newAcceleration
      PrevAcceleration = body.Acceleration },
  deltaPosition

let simulate otherSideEffects model timeToSimulate =
  let numSteps = timeToSimulate / timeStep |> floor |> int
  let mutable projectile = model.Projectile

  for step = 1 to numSteps do
    let newProjectile, deltaPosition = move projectile timeStep (Model.bodyAcceleration model projectile)
    otherSideEffects projectile deltaPosition (timeStep * float step) // Allows data in between frames to be accessed, in this case used to draw trajectory tracers
    projectile <- newProjectile

  let simulatedTime = timeStep * float numSteps
  let newTime = model.Time + simulatedTime
  { model with
      Time = newTime
      LeftOverTime = timeToSimulate - simulatedTime
      LastTracer = newTime - ((newTime - model.LastTracer) % model.TraceInterval)
      Projectile = projectile }
