model Thermal "Lumped thermal mass cooling toward ambient"
  parameter Real Rth = 2.0 "Thermal resistance [K/W]";
  parameter Real Cth = 3.0 "Thermal capacitance [J/K]";
  parameter Real Tamb = 20.0 "Ambient temperature [degC]";
  parameter Real T0 = 100.0 "Initial temperature [degC]";
  output Real T(start = T0) "Temperature [degC]";
equation
  Cth * der(T) = (Tamb - T) / Rth;
end Thermal;
