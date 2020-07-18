using System.Numerics;
using static System.Math;

namespace ProjectilePhysicsSimulation
{
    public class Force : IForce
    {
        public Vector<double> Vect(Body body) => new Vector<double>(new double[]
        {
            Magnitude * Cos(Degrees * PI / 180) * body.Mass,
            Magnitude * Sin(Degrees * PI / 180) * body.Mass
        });

        public double Degrees { get; set; }
        public double Magnitude { get; set; }

        public Force(double degrees, double magnitude)
        {
            Degrees = degrees;
            Magnitude = magnitude;
        }
    }
}
