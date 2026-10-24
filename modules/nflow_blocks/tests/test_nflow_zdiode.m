%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Zener diode ZDiode (Coselica MEAS_ZDiode): forward Shockley conduction plus a
% reverse breakdown branch at -Vz, i = Is(exp(vd/Vt)-1) - Is(exp(-(vd+Vz)/Vt)-1).
% Native physicalIsland kind zdiode (nonlinear, embedded-Newton).
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
% Zener shunt regulator: 10 V through a 100-ohm series R into a reverse-biased
% Zener (Vz = 5). Without the Zener the node would sit at 10 V; the breakdown
% clamps it to the Zener knee (Vz plus a small soft-knee offset), well below 10.
d = ['{"version":"1.0.0","format":"nflow-2","name":"Z","blocks":[', ...
  '{"id":"V1","type":"ConstantVoltage","params":{"V":10}},', ...
  '{"id":"R1","type":"Resistor","params":{"R":100}},', ...
  '{"id":"DZ","type":"ZDiode","params":{"Is":1e-9,"Vt":0.04,"Vz":5}},', ...
  '{"id":"G","type":"Ground","params":{}},', ...
  '{"id":"VS","type":"VoltageSensor","params":{}},', ...
  '{"id":"sc","type":"scope","inputs":1,"outputs":0,"params":{}}],', ...
  '"connections":[{"from":"VS","to":"sc","fromIndex":0,"toIndex":0}],', ...
  '"physicalWires":[{"pins":["V1.p","R1.p"]},{"pins":["R1.n","DZ.n","VS.p"]},', ...
  '{"pins":["V1.n","DZ.p","VS.n","G.p"]}],', ...
  '"solver":"dae","runtime":0.1,"sampleTime":0.02}'];
r = jsondecode(__nflow_simulate__(NFlow.internal.acausalExpand(d)));
assert_isequal(r.status, 'done');
v = r.scopes(1).series(end);
assert_istrue(v > 5.0 && v < 6.5); % clamped just above Vz, not 10 V
%=============================================================================
% Forward bias (source reversed): the Zener conducts like a normal diode, so the
% node sits at a small forward drop (below 1 V in magnitude).
d2 = strrep(strrep(d, '"V":10', '"V":-1'), '"R":100', '"R":10');
r = jsondecode(__nflow_simulate__(NFlow.internal.acausalExpand(d2)));
assert_isequal(r.status, 'done');
assert_istrue(abs(r.scopes(1).series(end)) < 1.0);
%=============================================================================
