module ProjectilePhysicsSimulation.Physics

open FSharp.Data.UnitSystems.SI.UnitSymbols

// milisecond
[<Measure>] type ms

// degrees
[<Measure>] type deg


// Here we have some fun and make our own Vector2 type:
[<Fable.Core.Erase>]
type Vector2<[<Measure>] 'u> =
  Vector2 of
    x: float<'u> *
    y: float<'u>
  with
    static member ( + ) (Vector2(ax, ay), Vector2(bx, by)) = Vector2(ax + bx, ay + by)
    static member ( - ) (Vector2(ax, ay), Vector2(bx, by)) = Vector2(ax - bx, ay - by)
    static member ( * ) (Vector2(ax, ay), Vector2(bx, by)) = ax * bx + ay * by
    static member ( * ) (scalar, Vector2(x, y)) = Vector2(scalar * x, scalar * y)
    static member ( * ) (Vector2(x, y), scalar) = Vector2(scalar * x, scalar * y)
    static member ( / ) (Vector2(x, y), scalar) = Vector2(x / scalar, y / scalar)


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

  let magnitude (vector: Vector2<_>) = sqrt (vector * vector)

  let normalizeUnsafe vector = vector / magnitude vector

  let normalize vector =
    if vector = zero
    then zero
    else normalizeUnsafe vector

type Vector2<[<Measure>] 'u> with
  member inline this.X = Vector2.X this
  member inline this.Y = Vector2.Y this




type Body =
  { Mass: float<kg>
    Dimensions: Vector2<m> // Width, Height
    Position: Vector2<m> // Bottom left corner
    Velocity: Vector2<m/s>
    Acceleration: Vector2<m/s^2>
    PrevAcceleration: Vector2<m/s^2> }

module Body =
  let minMass = System.Double.Epsilon * 1.0<kg>

  let center body = body.Position + body.Dimensions / 2.0




type Settings =
  { InitialSpeed: float<m/s>
    InitialAngle: float<deg>
    InitialPosition: Vector2<m>

    AccelerationGravity: float<m/s^2>
    DragConstant: float<kg/m>

    ShowTrajectory: bool
    ShowVelocityMarker: bool

    TraceInterval: float<s>
    SimulationSpeed: float
    JumpStep: float<s> }

type Simulation =
  { Projectile: Body
    Running: bool
    Time: float<s>
    LastTracer: float<s>
    LeftOverTime: float<s>
    Settings: Settings }

let timeStep = 1.0<s> / 120.0

module Simulation =
  let minSpeed = 0.0
  let maxSpeed = 10000.0
  let minJumpStep = timeStep
  let maxJumpStep = 10000.0<s>
  let minDragConstant = 0.0<kg/m>
  let minTraceInterval = timeStep - timeStep / 1000000000.0

  let started sim = sim.Time > 0.0<_>
  let timeSinceLastTracer sim =
    if not (started sim)
    then sim.Settings.TraceInterval
    else sim.Time - sim.LastTracer

  let private applyGravity (gravity: float<m/s^2>) (Vector2(x, y)) = Vector2(x, y + gravity)

  // Force drag = 1/2 * fluid density (kg/m^3) * contact area (m^2) * drag coefficient (unitless) * v^2 (m^2/s^2) * [direction Vector2 (unitless)]
  // https://en.wikipedia.org/wiki/Drag_%28physics%29
  // c (kg/m) = fluid density * contact area * drag coefficient / 2
  // Force drag (kg m/s^2) = c (kg/m) * v^2 (m^2/s^2) * [direction]
  let private accclerationDrag (constant: float<kg/m>) (velocity: Vector2<m/s>) (mass: float<kg>) =
    -constant * (velocity * velocity) * (Vector2.normalize velocity) / mass

  let acceleration settings body =
    accclerationDrag settings.DragConstant body.Velocity body.Mass
    |> applyGravity settings.AccelerationGravity

  // Uses Beeman's algortithm, a variation of velocity verlet (https://en.wikipedia.org/wiki/Beeman%27s_algorithm):
  let private move body (time: float<s>) newAcceleration =
    let deltaPosition = body.Velocity * time + 1.0 / 6.0 * body.Acceleration * (time * time)
    { body with
        Position = body.Position + deltaPosition
        Velocity = body.Velocity + 1.0 / 6.0 * (2.0 * newAcceleration + 5.0 * body.Acceleration - body.PrevAcceleration) * time
        Acceleration = newAcceleration
        PrevAcceleration = body.Acceleration },
    deltaPosition

  let simulate otherSideEffects sim timeToSimulate =
    let numSteps = timeToSimulate / timeStep |> floor |> int
    let mutable projectile = sim.Projectile

    for step = 1 to numSteps do
      let newProjectile, deltaPosition = move projectile timeStep (acceleration sim.Settings projectile)
      otherSideEffects projectile deltaPosition (timeStep * float step) // Allows data in between frames to be accessed, in this case used to draw trajectory tracers
      projectile <- newProjectile

    let simulatedTime = timeStep * float numSteps
    let newTime = sim.Time + simulatedTime
    { sim with
        Time = newTime
        LeftOverTime = timeToSimulate - simulatedTime
        LastTracer = newTime - ((newTime - sim.LastTracer) % sim.Settings.TraceInterval)
        Projectile = projectile }

  let initialProjectile settings projectile =
    { projectile with
        Position = settings.InitialPosition
        Velocity = Vector2.ofMagnitudeDegrees settings.InitialSpeed settings.InitialAngle
        Acceleration = acceleration settings projectile
        PrevAcceleration = Vector2.zero }

  let withSettings settings sim =
    { sim with
        Settings = settings
        Projectile = initialProjectile settings sim.Projectile }
