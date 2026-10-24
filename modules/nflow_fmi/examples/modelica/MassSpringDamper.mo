model MassSpringDamper "Mass-spring-damper released from an initial displacement"
  parameter Real m = 1.0 "Mass [kg]";
  parameter Real k = 1.0 "Spring stiffness [N/m]";
  parameter Real c = 0.4 "Damping coefficient [N.s/m]";
  parameter Real x0 = 1.0 "Initial displacement [m]";
  output Real x(start = x0) "Position [m]";
  Real v(start = 0.0) "Velocity [m/s]";
equation
  der(x) = v;
  m * der(v) = -k * x - c * v;
end MassSpringDamper;
