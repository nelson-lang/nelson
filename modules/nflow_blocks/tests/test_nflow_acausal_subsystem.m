%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% A self-contained acausal island inside a PLAIN subsystem is lowered to
% a nested physicalIsland and simulates. This pins the recursion through the
% subsystem boundary: the waveform pre-pass (a sine source inside a subsystem),
% two levels of nesting, and a mechanical (non-electrical) island.
%=============================================================================
function b = ac(id, type, params)
  b = struct('id', id, 'type', type, 'x', 0, 'y', 0, 'rotation', 0, 'params', params);
end
%=============================================================================
function sk = labelsink(id, tag)
  sk = struct('id', id, 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'x', 0, 'y', 0, 'rotation', 0, 'params', struct('gotoTag', tag));
end
%=============================================================================
function tw = toworkspace(id, name)
  tw = struct('id', id, 'type', 'toWorkspace', 'x', 0, 'y', 0, 'rotation', 0, ...
    'inputs', 1, 'outputs', 0, 'params', struct('VariableName', name, ...
    'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'));
end
%=============================================================================
function sub = plainsub(id, inner)
  sub = struct('id', id, 'type', 'subsystem', 'inputs', 0, 'outputs', 1, ...
    'x', 200, 'y', 120, 'rotation', 0, 'params', struct('subsystem', inner));
end
%=============================================================================
function y = sim_var(doc, sensorId, sensorTag, outName)
  % Wire the (already-built) inner diagram's sensor to a labelSink external
  % output, wrap it in a plain subsystem feeding a toWorkspace, simulate, and
  % return the recorded trace.
  doc.connections = {struct('from', sensorId, 'to', 'yo', 'fromIndex', 0, 'toIndex', 0)};
  doc.blocks{end + 1} = labelsink('yo', sensorTag);
  doc.externalOutputs = {struct('id', 'yo', 'name', sensorTag)};
  top = struct();
  top.version = '1.0.0';
  top.format = 'nflow-2';
  top.name = 'acausal_subsystem';
  top.runtime = 1;
  top.sampleTime = 0.01;
  top.solver = 'dae';
  top.blocks = {plainsub('sub', doc), toworkspace('tw', outName)};
  top.connections = {struct('from', 'sub', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
  top.physicalWires = {};
  m = [tempname(), '.nflow'];
  fid = fopen(m, 'wt');
  fwrite(fid, jsonencode(top));
  fclose(fid);
  sim(m);
  rmfile(m);
  y = evalin('base', outName);
  y = y(:);
end
%=============================================================================
% 1) A sine-driven RC inside a plain subsystem. The waveform pre-pass runs inside
%    the recursion (SineVoltage -> sine generator + SignalVoltage). Closed form:
%    v(t) = A/(1+(w tau)^2) (sin wt - w tau cos wt + w tau e^{-t/tau}),
%    A = 1, w = 2 pi, tau = R*C = 100*1e-3 = 0.1.
inner = struct();
inner.blocks = { ...
  ac('V1', 'SineVoltage', struct('Amplitude', 1, 'Frequency', 1, 'Phase', 0)), ...
  ac('R1', 'Resistor', struct('R', 100)), ...
  ac('C1', 'Capacitor', struct('C', 0.001, 'v0', 0)), ...
  ac('G', 'Ground', struct()), ...
  ac('VS', 'VoltageSensor', struct())};
inner.physicalWires = {struct('pins', {{'V1.p', 'R1.p'}}), ...
  struct('pins', {{'R1.n', 'C1.p', 'VS.p'}}), ...
  struct('pins', {{'V1.n', 'C1.n', 'VS.n', 'G.p'}})};
y = sim_var(inner, 'VS', 'vc', 'nfsub_sine');
t = (0:numel(y) - 1)' * 0.01;
w = 2 * pi;
tau = 0.1;
yref = (1 / (1 + (w * tau) ^ 2)) * (sin(w * t) - w * tau * cos(w * t) + w * tau * exp(-t / tau));
assert_istrue(numel(y) == numel(yref));
assert_istrue(max(abs(y - yref)) < 2e-3);
%=============================================================================
% 2) Two levels of nesting: the RC island lives in a plain subsystem nested
%    inside another plain subsystem. The recursion must lower it and thread the
%    signal out through both external outputs. Constant-source step, tau = 0.1.
lvl2 = struct();
lvl2.blocks = { ...
  ac('V1', 'ConstantVoltage', struct('V', 1)), ac('R1', 'Resistor', struct('R', 100)), ...
  ac('C1', 'Capacitor', struct('C', 0.001, 'v0', 0)), ac('G', 'Ground', struct()), ...
  ac('VS', 'VoltageSensor', struct()), labelsink('yo2', 'y2')};
lvl2.connections = {struct('from', 'VS', 'to', 'yo2', 'fromIndex', 0, 'toIndex', 0)};
lvl2.physicalWires = {struct('pins', {{'V1.p', 'R1.p'}}), ...
  struct('pins', {{'R1.n', 'C1.p', 'VS.p'}}), struct('pins', {{'V1.n', 'C1.n', 'VS.n', 'G.p'}})};
lvl2.externalOutputs = {struct('id', 'yo2', 'name', 'y2')};
lvl1 = struct();
lvl1.blocks = {plainsub('sub2', lvl2)};
% (sim_var appends the labelSink 'yo' + external output and wires 'sub2' to it.)
y2 = sim_var(lvl1, 'sub2', 'y1', 'nfsub_nested');
t = (0:numel(y2) - 1)' * 0.01;
yref2 = 1 - exp(-t / 0.1);
assert_istrue(numel(y2) == numel(yref2));
assert_istrue(max(abs(y2 - yref2)) < 1e-3);
%=============================================================================
% 3) A mechanical island (mass + spring) inside a plain subsystem: the recursion
%    is domain-agnostic. Undamped oscillator x'' = -(k/m) x, x0 = 0.1, v0 = 0 ->
%    x(t) = x0 cos(wn t), wn = sqrt(k/m) = sqrt(4/1) = 2.
innerM = struct();
innerM.blocks = { ...
  ac('M1', 'Mass', struct('m', 1, 's0', 0.1, 'v0', 0)), ...
  ac('K1', 'Spring', struct('k', 4)), ...
  ac('GM', 'Fixed', struct('s0', 0)), ...
  ac('PS', 'PositionSensor', struct())};
innerM.physicalWires = {struct('pins', {{'GM.flange', 'K1.a'}}), ...
  struct('pins', {{'K1.b', 'M1.flange', 'PS.flange'}})};
xm = sim_var(innerM, 'PS', 'xm', 'nfsub_mech');
tm = (0:numel(xm) - 1)' * 0.01;
xref = 0.1 * cos(2 * tm);
assert_istrue(numel(xm) == numel(xref));
assert_istrue(max(abs(xm - xref)) < 5e-3);
%=============================================================================
% 4) The GUI-openable example diagram (acausal RC low-pass grouped in a plain
%    subsystem) loads, lowers through the subsystem recursion and simulates to
%    completion, matching the same sine-RC closed form as case 1 (tau = 0.1).
exf = [modulepath('nflow_blocks'), '/examples/acausal/Acausal_Subsystem_RC_Lowpass.nflow'];
sim(exf);
ve = evalin('base', 'vc');
ve = ve(:);
te = (0:numel(ve) - 1)' * 0.01;
vre = (1 / (1 + (w * tau) ^ 2)) * (sin(w * te) - w * tau * cos(w * te) + w * tau * exp(-te / tau));
assert_istrue(numel(ve) == numel(vre));
assert_istrue(max(abs(ve - vre)) < 2e-3);
%=============================================================================
