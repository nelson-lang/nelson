model RLC_MultiOutput "Series R-L-C exposing two measured outputs (voltage and current)"
  parameter Real R = 1.0 "Resistance [Ohm]";
  parameter Real L = 1.0 "Inductance [H]";
  parameter Real C = 1.0 "Capacitance [F]";
  parameter Real V = 1.0 "Source voltage [V]";
  // NOTE: the FMU (and therefore the nflow block) orders output ports by the
  // FMU's variable order, not by this declaration order. Query the port names
  // with fmiInfo on the compiled FMU (or read them in the block inspector).
  output Real vC(start = 0.0) "Capacitor voltage [V]";
  output Real iL(start = 0.0) "Inductor current [A]";
equation
  L * der(iL) = V - R * iL - vC;
  C * der(vC) = iL;
end RLC_MultiOutput;
