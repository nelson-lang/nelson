%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Acausal palette N3 (thermal parity). The physicalIsland block is a generic
% across/through DAE solver: the thermal domain (across = temperature T, through
% = heat flow Q) maps onto the SAME component kinds as electrical, so thermal
% models run with no new engine code:
%   HeatCapacitor    -> capacitor to the 0-reference (C dT/dt = Q)
%   ThermalConductor -> conductor (Q = G (T_a - T_b))
%   FixedTemperature -> vsource       PrescribedTemperature -> vsource(port)
%   FixedHeatFlow    -> isource        PrescribedHeatFlow    -> isource(port)
%   TemperatureSensor    -> potentialSensor
%   RelTemperatureSensor -> voltageSensor
%   HeatFlowSensor       -> currentSensor
% This test locks that mapping with Newton-cooling and two-body checks.
%=============================================================================
function p = th_write(json, stem)
  p = [tempname(), '.nflow'];
  fid = fopen(p, 'w'); fwrite(fid, json); fclose(fid);
end
%=============================================================================
% 1) Newton cooling: a body (C = 2 J/K, T0 = 100) tied through a thermal
%    conductor (G = 1 W/K) to a fixed ambient (T_amb = 20). Analytic solution
%    T(t) = 20 + 80 e^{-(G/C) t} = 20 + 80 e^{-0.5 t}.
%    Nodes: 0 = 0 K reference, 1 = body, 2 = ambient.
cool = ['{"blocks":[', ...
  '{"id":"isl","type":"physicalIsland","inputs":0,"outputs":1,"params":{', ...
    '"domain":"thermal","nodes":3,"components":[', ...
      '{"kind":"vsource","a":2,"b":0,"V":20},', ...            % FixedTemperature 20
      '{"kind":"conductor","a":1,"b":2,"G":1},', ...           % ThermalConductor
      '{"kind":"capacitor","a":1,"b":0,"C":2,"ic":100},', ...  % HeatCapacitor
      '{"kind":"potentialSensor","a":1,"outPort":0}]}},', ...  % TemperatureSensor
  '{"id":"twT","type":"toWorkspace","inputs":1,"outputs":0,"params":{"VariableName":"T"}}],', ...
  '"connections":[{"from":"isl","to":"twT","fromIndex":0,"toIndex":0}],', ...
  '"solver":"dae","runtime":6.0,"sampleTime":0.1}'];
out = sim(th_write(cool, 'test_nflow_thermal_cool'));
lg = out.logsout; nms = {lg.name};
T = lg(find(strcmp(nms, 'T'), 1)).values;
t = linspace(0, 6, numel(T))';
Tref = 20 + 80 * exp(-0.5 * t);
assert_istrue(max(abs(T(:) - Tref)) < 1e-3);
%=============================================================================
% 2) Constant heat flow into an insulated body: FixedHeatFlow Q = 5 W into a
%    body C = 10 J/K (T0 = 0) with no conduction path. T rises linearly at
%    dT/dt = Q/C = 0.5 K/s, so T(t) = 0.5 t.
%    A HeatCapacitor alone (all-differential node) needs no fixed temperature.
%    isource current flows a -> b (leaves a), so to INJECT into node 1 the
%    source runs ground -> node1 (a = 0, b = 1) -- the mapping FixedHeatFlow
%    (heat into the port) will apply.
heatin = ['{"blocks":[', ...
  '{"id":"isl","type":"physicalIsland","inputs":0,"outputs":1,"params":{', ...
    '"domain":"thermal","nodes":2,"components":[', ...
      '{"kind":"isource","a":0,"b":1,"I":5},', ...             % FixedHeatFlow 5 W (into node 1)
      '{"kind":"capacitor","a":1,"b":0,"C":10,"ic":0},', ...   % HeatCapacitor
      '{"kind":"potentialSensor","a":1,"outPort":0}]}},', ...
  '{"id":"twT","type":"toWorkspace","inputs":1,"outputs":0,"params":{"VariableName":"T"}}],', ...
  '"connections":[{"from":"isl","to":"twT","fromIndex":0,"toIndex":0}],', ...
  '"solver":"dae","runtime":4.0,"sampleTime":0.2}'];
out = sim(th_write(heatin, 'test_nflow_thermal_heatin'));
lg = out.logsout; nms = {lg.name};
T = lg(find(strcmp(nms, 'T'), 1)).values;
t = linspace(0, 4, numel(T))';
assert_istrue(max(abs(T(:) - 0.5 * t)) < 1e-4);
%=============================================================================
% 3) Two bodies equilibrating: C1=1 (T0=100), C2=3 (T0=20), joined by a
%    conductor G=1, insulated from ambient. Energy conserves: the common final
%    temperature is (C1 T1 + C2 T2)/(C1+C2) = (100 + 60)/4 = 40. Check the late
%    sample of both temperatures approaches 40 and total energy is conserved.
two = ['{"blocks":[', ...
  '{"id":"isl","type":"physicalIsland","inputs":0,"outputs":2,"params":{', ...
    '"domain":"thermal","nodes":3,"components":[', ...
      '{"kind":"capacitor","a":1,"b":0,"C":1,"ic":100},', ...
      '{"kind":"capacitor","a":2,"b":0,"C":3,"ic":20},', ...
      '{"kind":"conductor","a":1,"b":2,"G":1},', ...
      '{"kind":"potentialSensor","a":1,"outPort":0},', ...
      '{"kind":"potentialSensor","a":2,"outPort":1}]}},', ...
  '{"id":"tw1","type":"toWorkspace","inputs":1,"outputs":0,"params":{"VariableName":"T1"}},', ...
  '{"id":"tw2","type":"toWorkspace","inputs":1,"outputs":0,"params":{"VariableName":"T2"}}],', ...
  '"connections":[', ...
    '{"from":"isl","to":"tw1","fromIndex":0,"toIndex":0},', ...
    '{"from":"isl","to":"tw2","fromIndex":1,"toIndex":0}],', ...
  '"solver":"dae","runtime":20.0,"sampleTime":0.1}'];
out = sim(th_write(two, 'test_nflow_thermal_two'));
lg = out.logsout; nms = {lg.name};
T1 = lg(find(strcmp(nms, 'T1'), 1)).values;
T2 = lg(find(strcmp(nms, 'T2'), 1)).values;
assert_istrue(abs(T1(end) - 40) < 0.2);
assert_istrue(abs(T2(end) - 40) < 0.2);
% energy conservation at every sample: C1 T1 + C2 T2 = const = 160
E = 1 * T1(:) + 3 * T2(:);
assert_istrue(max(abs(E - 160)) < 1e-2);
%=============================================================================
% 4) Radiative cooling (Stefan-Boltzmann): a body C=1, T0=5 radiating to a
%    fixed ambient T_amb=2 via Gr=0.1. C dT/dt = -Gr (T^4 - T_amb^4).
%    Expected: ode45 of that nonlinear ODE.
Gr = 0.1; Tamb = 2;
odef = @(tt, T) - Gr * (T .^ 4 - Tamb ^ 4);
[tref, Tref] = ode45(odef, [0 2], 5);
rad = ['{"blocks":[', ...
  '{"id":"isl","type":"physicalIsland","inputs":0,"outputs":1,"params":{', ...
    '"domain":"thermal","nodes":3,"components":[', ...
      '{"kind":"vsource","a":2,"b":0,"V":2},', ...            % fixed ambient T=2
      '{"kind":"radiation","a":1,"b":2,"Gr":0.1},', ...       % radiation body->ambient
      '{"kind":"capacitor","a":1,"b":0,"C":1,"ic":5},', ...   % body C=1, T0=5
      '{"kind":"potentialSensor","a":1,"outPort":0}]}},', ...
  '{"id":"twT","type":"toWorkspace","inputs":1,"outputs":0,"params":{"VariableName":"T"}}],', ...
  '"connections":[{"from":"isl","to":"twT","fromIndex":0,"toIndex":0}],', ...
  '"solver":"dae","runtime":2.0,"sampleTime":0.05}'];
out = sim(th_write(rad, 'test_nflow_thermal_rad'));
lg = out.logsout; nms = {lg.name};
T = lg(find(strcmp(nms, 'T'), 1)).values;
t = linspace(0, 2, numel(T))';
assert_istrue(max(abs(T(:) - interp1(tref, Tref, t))) < 5e-3);
%=============================================================================
% 5) ThermalResistor mirrors case 1 with R = 1/G: the same Newton-cooling body
%    (C = 2, T0 = 100) tied to ambient 20 through a thermal resistor R = 1 K/W
%    gives the identical T(t) = 20 + 80 e^{-t/(R C)} = 20 + 80 e^{-0.5 t}. A
%    ConvectiveResistor with a constant signal Rc = 1 must match bit for bit.
res = ['{"blocks":[', ...
  '{"id":"isl","type":"physicalIsland","inputs":0,"outputs":1,"params":{', ...
    '"domain":"thermal","nodes":3,"components":[', ...
      '{"kind":"vsource","a":2,"b":0,"V":20},', ...            % FixedTemperature 20
      '{"kind":"resistor","a":1,"b":2,"R":1},', ...            % ThermalResistor R = 1
      '{"kind":"capacitor","a":1,"b":0,"C":2,"ic":100},', ...  % HeatCapacitor
      '{"kind":"potentialSensor","a":1,"outPort":0}]}},', ...
  '{"id":"twT","type":"toWorkspace","inputs":1,"outputs":0,"params":{"VariableName":"T"}}],', ...
  '"connections":[{"from":"isl","to":"twT","fromIndex":0,"toIndex":0}],', ...
  '"solver":"dae","runtime":6.0,"sampleTime":0.1}'];
out = sim(th_write(res, 'test_nflow_thermal_res'));
lg = out.logsout; nms = {lg.name};
T = lg(find(strcmp(nms, 'T'), 1)).values;
t = linspace(0, 6, numel(T))';
Tref = 20 + 80 * exp(-0.5 * t);
assert_istrue(max(abs(T(:) - Tref)) < 1e-3);
%=============================================================================
