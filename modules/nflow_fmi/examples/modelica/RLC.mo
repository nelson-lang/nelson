model RLC "Series R-L-C circuit driven by a DC source"
  parameter Real R = 1.0 "Resistance [Ohm]";
  parameter Real L = 1.0 "Inductance [H]";
  parameter Real C = 1.0 "Capacitance [F]";
  parameter Real V = 1.0 "Source voltage [V]";
  output Real vC(start = 0.0) "Capacitor voltage [V]";
  Real iL(start = 0.0) "Inductor current [A]";
equation
  L * der(iL) = V - R * iL - vC;
  C * der(vC) = iL;
end RLC;
