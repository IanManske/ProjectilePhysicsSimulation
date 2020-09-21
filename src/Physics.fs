module ProjectilePhysicsSimulation.Physics

open System
open FSharp.Data.UnitSystems.SI.UnitSymbols

// Not sure if System.Numerics.Vector2 works well with units of measure.
// So here we have some fun and make our own:
type Vector2D<[<Measure>]'u> =
  { X: float<'u>
    Y: float<'u> }

// Currently a problem with the fable complier, as overloaded operators don't always work (https://github.com/fable-compiler/Fable/issues/2101):
// So here we have to use global operators instead of "static member ({operator}) ({args})":
let inline (.+) a b =
  { X = a.X + b.X
    Y = a.Y + b.Y }

let inline (.-) a b =
  { X = a.X - b.X
    Y = a.Y - b.Y }

let inline (.*) scalar vector =
  { X = scalar * vector.X
    Y = scalar * vector.Y }

let inline (..*) vector scalar =
  { X = vector.X * scalar
    Y = vector.Y * scalar }

// dot product
let inline (.*.) a b = a.X * b.X + a.Y * b.Y

// element-wise multiplication (not needed)
//let inline (..*..) a b =
//  { X = a.X * b.X
//    Y = a.Y * b.Y }

// cross product (not needed)
//let inline (><) a b = ...

let inline (./) vector scalar =
  { X = vector.X / scalar
    Y = vector.Y / scalar }

let zero2D =
  { X = 0.0<_>
    Y = 0.0<_> }

let magnitude vector = sqrt (vector .*. vector)

let normalize vector = vector ./ magnitude vector

let lerpDelta a delta alpha = a .+ alpha .* delta

//let lerp a b = lerpDelta a (b .- a)

// Returns the vector with the lower magnitude.
let min2D a b =
  if magnitude a < magnitude b
  then a
  else b

//let max2D a b =
  //if magnitude a > magnitude b
  //then a
  //else b

// milisecond
[<Measure>] type ms
// degrees
[<Measure>] type deg

let magnitudeDegreesToVector (magnitude: float<'u>) (degrees: float<deg>) =
  { X = magnitude * cos (degrees * Math.PI / 180.0<deg>)
    Y = magnitude * sin (degrees * Math.PI / 180.0<deg>) }

type Body =
  { Width: int<m>
    Height: int<m>
    Mass: float<kg>
    Position: Vector2D<m>
    Velocity: Vector2D<m/s>
    Acceleration: Vector2D<m/s^2>
    PrevAcceleration: Vector2D<m/s^2> }

let center body =
  { X = body.Position.X + float body.Width * 0.5<_>
    Y = body.Position.Y + float body.Height * 0.5<_> }

let applyGravity (gravity: float<m/s^2>) acceleration = { acceleration with Y = acceleration.Y + gravity }

// Force drag = 1/2 * fluid density (kg/m^3) * contact area (m^2) * drag coefficient (unitless) * v^2 (m^2/s^2) * [direction vector (unitless)]
// https://en.wikipedia.org/wiki/Drag_%28physics%29
// c (kg/m) = fluid density * contact area * drag coefficient / 2
// Force drag (kg m/s^2) = c (kg/m) * v^2 (m^2/s^2) * [direction]
let accclerationDrag (constant: float<kg/m>) (velocity: Vector2D<m/s>) mass =
  min2D (-1.0<_> .* velocity) (-constant * (velocity .*. velocity) .* normalize velocity ./ mass)

// If Force drag is too large, then the velocity may suddenly change to negative value on the next frame when it should stop at 0.
// It would then flip back to being positive on the frame after and continue this pattern, essentially breaking the simulation.
// This can happen because the time step may not be small enough; it applies the acceleration all in one go, when in reality it would be gradual and decreasing.
// Because of this, the acceleration due to drag is clamped to not be larger in magnitude than the velocity (thereby fixing this bug),
// but the projectile probably takes longer to slow down in the simulation than in reality in the cases where the clamping comes into effect.

let timeStep = 1.0<s> / 120.0

type Model =
  { Projectile: Body
    InitialSpeed: float<m/s>
    InitialAngle: float<deg>
    InitialPosition: Vector2D<m>
    AccelerationGravity: float<m/s^2>
    DragConstant: float<kg/m>
    ShowTrajectory: bool
    ShowVelocityMarker: bool
    TraceInterval: float<s>
    LastTracer: float<s>
    Started: bool
    Running: bool
    SimulationSpeed: float
    JumpStep: float<s>
    Time: float<s>
    LeftOverTime: float<s> }

let calcAcceleration gravity dragConstant velocity mass =
  accclerationDrag dragConstant velocity mass
  |> applyGravity gravity

let calcAccelerationUsing model = calcAcceleration model.AccelerationGravity model.DragConstant

let calcBodyAcceleration gravity dragConstant body = calcAcceleration gravity dragConstant body.Velocity body.Mass

let calcBodyAccelerationUsing model = calcBodyAcceleration model.AccelerationGravity model.DragConstant

// Uses Beeman's algortithm, a variation of velocity verlet (https://en.wikipedia.org/wiki/Beeman's_algorithm):
let move body (time: float<s>) newAcceleration =
  let deltaPosition = body.Velocity ..* time .+ 1.0 / 6.0 .* body.Acceleration ..* (time * time)
  { body with
      Position = body.Position .+ deltaPosition
      Velocity = body.Velocity .+ 1.0 / 6.0 .* (2.0 .* newAcceleration .+ 5.0 .* body.Acceleration .- body.PrevAcceleration) ..* time
      Acceleration = newAcceleration
      PrevAcceleration = body.Acceleration },
  deltaPosition

let simulate model time =
  let numSteps = time / timeStep |> floor |> int
  let mutable step = 1
  let mutable tracers = 1
  let mutable tracerPositions = []
  let mutable timeAfterTracer = 0.0<_>
  let mutable projectile = model.Projectile
  while step <= numSteps do
    let newProjectile, deltaPosition = move projectile timeStep (calcBodyAccelerationUsing model projectile)
    timeAfterTracer <- model.Time + timeStep * float step - model.LastTracer - model.TraceInterval * float tracers
    if timeAfterTracer >= 0.0<_> then
      // alpha: how far into the timeStep the tracer should be drawn. [ 0 = start of timeStep, 1 = end of timeStep ]
      // Blend / perform a linear interpolation based on alpha:
      tracerPositions <- lerpDelta (center projectile) deltaPosition ((timeStep - timeAfterTracer) / timeStep)::tracerPositions
      tracers <- tracers + 1
    projectile <- newProjectile
    step <- step + 1

  let newTime = model.Time + timeStep * float numSteps

  { model with
      Started = true
      Time = newTime
      LeftOverTime = time - timeStep * float numSteps
      LastTracer = newTime - ((newTime - model.LastTracer) % model.TraceInterval)
      Projectile = projectile },
  tracerPositions