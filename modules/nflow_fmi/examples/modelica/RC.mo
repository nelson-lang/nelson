model RC "First-order RC circuit charging from a DC source"
  parameter Real R = 1.0 "Resistance [Ohm]";
  parameter Real C = 1.0 "Capacitance [F]";
  parameter Real V = 1.0 "Source voltage [V]";
  output Real vC(start = 0.0) "Capacitor voltage [V]";
equation
  R * C * der(vC) = V - vC;
end RC;
