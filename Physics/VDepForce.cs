using System.Numerics;
using static System.Math;

namespace ProjectilePhysicsSimulation
{
    public class VDepForce : IForce
    {
        public Vector<double> Vect(Body body) => -1 * body.Velocity * Constant;

        public double Constant
        {
            get => _Constant;
            set => _Constant = Max(value, 0);
        }
        double _Constant;

        public VDepForce(double constant)
        {
            Constant = constant;
        }
    }
}