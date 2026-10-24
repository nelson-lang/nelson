model BouncingBall "Ball bouncing on the floor with restitution (events)"
  parameter Real e = 0.8 "Coefficient of restitution [-]";
  parameter Real g = 9.81 "Gravity [m/s2]";
  parameter Real h0 = 1.0 "Initial height [m]";
  output Real h(start = h0) "Height [m]";
  Real v(start = 0.0) "Vertical velocity [m/s]";
equation
  der(h) = v;
  der(v) = -g;
  when h <= 0 then
    reinit(v, -e * pre(v));
  end when;
end BouncingBall;
