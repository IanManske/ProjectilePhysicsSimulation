using System.Collections.Generic;
using System.Numerics;
using static System.Math;

namespace ProjectilePhysicsSimulation
{
    public class Body
    {
        public double Width { get; }
        public double Height { get; }
        public double Mass
        {
            get => _Mass;
            set => _Mass = Max(value, 0);
        }
        double _Mass;

        //Vector2D could be used, but its data is of type float (less accurate than double)
        public Vector<double> Position
        {
            get => _Position;
            set 
            {
                PrevPosition = _Position;
                _Position = value;
            }
        }
        Vector<double> _Position;
        Vector<double> PrevPosition;
        public double X => Position[0];
        public double Y => Position[1];
        public double PrevX => PrevPosition[0];
        public double PrevY => PrevPosition[1];

        public Vector<double> Velocity
        {
            get => _Velocity;
            set 
            {
                PrevVelocity = _Velocity;
                _Velocity = value;
            }
        }
        Vector<double> _Velocity;
        Vector<double> PrevVelocity;
        public double VX => Velocity[0];
        public double VY => Velocity[1];
        public double PrevVX => PrevVelocity[0];
        public double PrevVY => PrevVelocity[1];

        public Vector<double> Acceleration
        {
            get => _Acceleration;
            set
            {
                PrevAcceleration = _Acceleration;
                _Acceleration = value;
            }
        }
        Vector<double> _Acceleration;
        Vector<double> PrevAcceleration;
        public Vector<double> NewAcceleration
        {
            get
            {
                Vector<double> sum = new Vector<double>(new double[] { 0, 0 });
                foreach(IForce force in Forces)
                {
                    sum += force.Vect(this);
                }
                return sum * (1 / Mass);
            }
        }
        public double AX => NewAcceleration[0];
        public double AY => NewAcceleration[1];
        readonly List<IForce> Forces;
        

        public void Move(double deltaTime)
        {
            // Old implementation:
            //Position += Velocity * deltaTime;
            //Velocity += Acceleration * deltaTime;
            //Acceleration = NewAcceleration;

            // New Velocity Verlet implementation (more accurate):
            //Position += (Velocity + Acceleration * deltaTime * 0.5) * deltaTime;
            //Velocity += (Acceleration + NewAcceleration) * deltaTime * 0.5;
            //Acceleration = NewAcceleration;

            // Using Beeman's algorithm (same positions, but more accurate velocities) (a variation of Velocity Verlet):
            // The difference from Velocity Verlet is only noticable with non-constant acceleration (e.g. air friction)
            Position += (Velocity + (1 / 6.0) * (4 * Acceleration - PrevAcceleration) * deltaTime) * deltaTime;
            Velocity += (1 / 6.0) * (2 * NewAcceleration + 5 * Acceleration - PrevAcceleration) * deltaTime;
            Acceleration = NewAcceleration;
        }

        public Body(double width, double height, double mass, Vector<double> position, Vector<double> velocity, List<IForce> forces = null)
        {
            Width = width;
            Height = height;
            Mass = mass;
            Forces = forces ?? new List<IForce>();
            //set values twice to also set previous values;
            Position = position;
            Position = Position;
            Velocity = velocity;
            Velocity = Velocity;
            _Acceleration = NewAcceleration;
            Acceleration = Acceleration;
        }
    }
}
