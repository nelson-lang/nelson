model DCMotor "DC motor driven by a constant armature voltage"
  parameter Real R = 1.0 "Armature resistance [Ohm]";
  parameter Real L = 0.5 "Armature inductance [H]";
  parameter Real J = 0.01 "Rotor inertia [kg.m2]";
  parameter Real b = 0.1 "Viscous friction [N.m.s]";
  parameter Real Kt = 0.1 "Torque constant [N.m/A]";
  parameter Real Ke = 0.1 "Back-EMF constant [V.s/rad]";
  parameter Real V = 1.0 "Armature voltage [V]";
  output Real w(start = 0.0) "Angular speed [rad/s]";
  Real i(start = 0.0) "Armature current [A]";
equation
  L * der(i) = V - R * i - Ke * w;
  J * der(w) = Kt * i - b * w;
end DCMotor;
