using System.Numerics;

namespace ProjectilePhysicsSimulation
{
    public interface IForce
    {
        Vector<double> Vect(Body body);
    }
}