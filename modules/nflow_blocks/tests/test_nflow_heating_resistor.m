%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% HeatingResistor (Coselica): an electrical resistor that dissipates its power
% P = v^2 / R as heat into a thermal port. First cross-domain block, delivered
% by the NFlow.internal.acausalExpand pre-pass without an engine change: the block is
% split into an electrical Resistor + VoltageSensor, a causal chain (square then
% gain 1/R) forming P, and a thermal PrescribedHeatFlow driven by P. The coupling
% flows one-way (electrical -> thermal) through the causal signal layer.
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
% 10 V across a 100-ohm HeatingResistor dissipates P = 100/100 = 1 W. That heat
% flows into a 1 J/K heat capacitor (referenced to 0), so dT/dt = 1 K/s: after
% 2 s the temperature has risen ~2 K above the 293.15 K start.
d = ['{"version":"1.0.0","format":"nflow-2","name":"HR","blocks":[', ...
  '{"id":"V1","type":"ConstantVoltage","params":{"V":10}},', ...
  '{"id":"HR","type":"HeatingResistor","params":{"R":100}},', ...
  '{"id":"G","type":"Ground","params":{}},', ...
  '{"id":"C1","type":"HeatCapacitor","params":{"C":1,"T0":293.15}},', ...
  '{"id":"TS","type":"TemperatureSensor","params":{}},', ...
  '{"id":"sc","type":"scope","inputs":1,"outputs":0,"params":{}}],', ...
  '"connections":[{"from":"TS","to":"sc","fromIndex":0,"toIndex":0}],', ...
  '"physicalWires":[{"pins":["V1.p","HR.p"]},', ...
  '{"pins":["V1.n","HR.n","G.p"]},', ...
  '{"pins":["HR.heatPort","C1.port","TS.port"]}],', ...
  '"solver":"dae","runtime":2.0,"sampleTime":0.05}'];
r = jsondecode(__nflow_simulate__(NFlow.internal.acausalExpand(d)));
assert_isequal(r.status, 'done');
T = r.scopes(1).series;
assert_istrue(T(end) > 294.9 && T(end) < 295.9); % risen ~2 K to ~295.15 K
assert_istrue(T(end) > T(1) + 1.5); % monotonic heating
%=============================================================================
% Doubling the voltage quadruples the power (P = v^2 / R), so the final
% temperature rise is about four times larger.
d2 = strrep(d, '"V":10', '"V":20');
r2 = jsondecode(__nflow_simulate__(NFlow.internal.acausalExpand(d2)));
assert_isequal(r2.status, 'done');
rise1 = T(end) - 293.15;
rise2 = r2.scopes(1).series(end) - 293.15;
assert_istrue(abs(rise2 / rise1 - 4.0) < 0.5); % 4x power -> 4x rise
%=============================================================================
