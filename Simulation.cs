using System;
using System.Collections.Generic;
using System.Timers;
using System.Numerics;
using static System.Math;

namespace ProjectilePhysicsSimulation
{
    public class Simulation
    {
        public int Width { get; }
        public int Height { get; }
        public Body Projectile { get; }
        public Force Gravity { get; }
        public VDepForce AirResistance { get; }

        public double InitialX
        {
            get => _X;
            set
            {
                _X = value;
                SetPosition();
            }
        }
        public double InitialY
        {
            get => _Y;
            set
            {
                _Y = value;
                SetPosition();
            }
        }
        void SetPosition()
        {
            Projectile.Position = InitialPosition;
            PositionChanged?.Invoke(Projectile);
        }
        Vector<double> InitialPosition => new Vector<double>(new double[] 
        {
            _X,
            Height - Projectile.Height - _Y
        });
        double _X;
        double _Y;

        public double InitialSpeed
        {
            get => _Speed;
            set
            {
                _Speed = value;
                SetVelocity();
            }
        }
        public double InitialAngle
        {
            get => -1 * _Angle;
            set
            {
                _Angle = -1 * value;
                SetVelocity();
            }
        }
        void SetVelocity()
        {
            Projectile.Velocity = InitialVelocity;
            VelocityChanged?.Invoke(Projectile);
        }
        Vector<double> InitialVelocity => new Vector<double>(new double[] 
        {
            _Speed * Cos(_Angle * PI / 180),
            _Speed * Sin(_Angle * PI / 180)
        });
        double _Speed;
        double _Angle;

        public bool ProjectileOutOfBounds
        => Projectile.X > Width
        || Projectile.Y > Height
        || Projectile.X + Projectile.Width < 0
        || Projectile.Y + Projectile.Height < 0;
        public bool Started { get; private set; }
        public bool Running => Timer.Enabled;
        public int Frame { get; private set; }
        public double Time { get; private set; }
        public int FPS //how many frames will the simulation try to do per second
        {
            get => _FPS;
            set
            {
                _FPS = Max(value, 5);
                Speed = SimulationSpeed; //recalc simulation speed
                Timer.Interval = TickSpeed;
            }
        }
        public double TickSpeed => 1000 / _FPS;
        public double Speed //how fast (but also more inaccurately) should the simulation run
        {
            get => SimulationSpeed;
            set
            {
                SimulationSpeed = Max(value, 0.01);
                SimulationSecondsPerFrame = SimulationSpeed / _FPS;
                TrajectoryTracerEverySecond = SecondsPerTrajectoryTracer; //recalc ticksPerSecond 
            }
        }
        public double TrajectoryTracerEverySecond //the simulation should draw a trajectory tracer every __ simulated seconds
        {
            get => SecondsPerTrajectoryTracer;
            set => SecondsPerTrajectoryTracer = Max(value, SimulationSecondsPerFrame + 0.0000000000001); //weird things happen if it is exactly equal to secondsPerFrame
        }

        public event Action StatsChanged;
        public event Action<Body> PositionChanged;
        public event Action<Body> VelocityChanged;
        public event Action<Body> TrajectoryTracer;

        int _FPS;
        double SimulationSpeed;
        double SimulationSecondsPerFrame;
        double SecondsPerTrajectoryTracer;
        double NextTrajectoryTracer;
        readonly Timer Timer;

        public void StartStop()
        {
            Timer.Enabled = !Timer.Enabled;
        }
        public void Reset()
        {
            Timer.Stop();
            //set values twice to reset previous position/velocity;
            Projectile.Position = InitialPosition;
            Projectile.Position = InitialPosition;
            Projectile.Velocity = InitialVelocity;
            Projectile.Velocity = InitialVelocity;
            Started = false;
            Frame = 0;
            Time = 0;
            NextTrajectoryTracer = 0;
        }
        public void Tick()
        {
            if(!Started)
            {
                Started = true;
                Projectile.Acceleration = Projectile.NewAcceleration;
                Projectile.Acceleration = Projectile.NewAcceleration;
            }
            if (ProjectileOutOfBounds)
            {
                Projectile.Move(SimulationSecondsPerFrame);
                if (!ProjectileOutOfBounds)
                {
                    TriggerUpdate();
                }
            }
            else
            {
                Projectile.Move(SimulationSecondsPerFrame);
                TriggerUpdate();
            }
            Frame++;
            Time += SimulationSecondsPerFrame;
            StatsChanged?.Invoke();
        }

        void TriggerUpdate()
        {
            PositionChanged?.Invoke(Projectile);
            if (Time >= NextTrajectoryTracer)
            {
                TrajectoryTracer?.Invoke(Projectile);
                NextTrajectoryTracer = Time + SecondsPerTrajectoryTracer;
            }
        }

        public Simulation()
        {
            Width = 1000;
            Height = 700;

            _FPS = 120;
            Timer = new Timer(TickSpeed);
            Timer.Elapsed += (s, e) => Tick();
            Speed = 5;
            SecondsPerTrajectoryTracer = 1;

            Gravity = new Force(90, 10);
            AirResistance = new VDepForce(0);
            List<IForce> forces = new List<IForce>()
            {
                Gravity,
                AirResistance
            };

            _X = 15;
            _Y = 15;
            _Speed = 120;
            _Angle = -70;

            double length = 25;
            Projectile = new Body(
                length,
                length,
                10,
                new Vector<double>(new double[]
                {
                    _X,
                    Height - length - _Y
                }),
                InitialVelocity,
                forces);
        }
    }
}
