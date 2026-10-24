%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Speed-dependent resistances to ground (Coselica): a linear F = -d v and a
% quadratic F = -d v |v| force on a flange, plus the rotational torque twins.
% Native mechanicalTranslationalIsland kinds (linearSpeedForce /
% quadraticSpeedForce), so they simulate through the DAE path.
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
% Linear: m dv/dt = -d v with v0=10, d=m=1 -> v(t) = 10 e^{-t}; v(3) = 0.498.
d = ['{"version":"1.0.0","format":"nflow-2","name":"L","blocks":[', ...
  '{"id":"M","type":"Mass","params":{"m":1,"s0":0,"v0":10}},', ...
  '{"id":"F","type":"LinearSpeedDependentForce","params":{"d":1}},', ...
  '{"id":"VS","type":"SpeedSensor","params":{}},', ...
  '{"id":"sc","type":"scope","inputs":1,"outputs":0,"params":{}}],', ...
  '"connections":[{"from":"VS","to":"sc","fromIndex":0,"toIndex":0}],', ...
  '"physicalWires":[{"pins":["M.flange","F.flange","VS.flange"]}],', ...
  '"solver":"dae","runtime":3,"sampleTime":0.02}'];
r = jsondecode(__nflow_simulate__(NFlow.internal.acausalExpand(d)));
assert_isequal(r.status, 'done');
v = r.scopes(1).series(:);
assert_isapprox(v(1), 10.0, 1e-3);
assert_isapprox(v(end), 10 * exp(-3), 5e-3); % exponential decay
%=============================================================================
% Quadratic: m dv/dt = -d v|v| -> v(t) = v0 / (1 + v0 d t); v(3) = 10/31.
d = strrep(d, 'LinearSpeedDependentForce', 'QuadraticSpeedDependentForce');
r = jsondecode(__nflow_simulate__(NFlow.internal.acausalExpand(d)));
assert_isequal(r.status, 'done');
v = r.scopes(1).series(:);
assert_isapprox(v(end), 10 / (1 + 10 * 1 * 3), 5e-3); % drag decay
%=============================================================================
% Rotational torque twin: J dw/dt = -d w with w0=10 -> w(3) = 10 e^{-3}.
d = ['{"version":"1.0.0","format":"nflow-2","name":"R","blocks":[', ...
  '{"id":"J","type":"Inertia","params":{"J":1,"phi0":0,"w0":10}},', ...
  '{"id":"T","type":"LinearSpeedDependentTorque","params":{"d":1}},', ...
  '{"id":"WS","type":"RotSpeedSensor","params":{}},', ...
  '{"id":"sc","type":"scope","inputs":1,"outputs":0,"params":{}}],', ...
  '"connections":[{"from":"WS","to":"sc","fromIndex":0,"toIndex":0}],', ...
  '"physicalWires":[{"pins":["J.flange","T.flange","WS.flange"]}],', ...
  '"solver":"dae","runtime":3,"sampleTime":0.02}'];
r = jsondecode(__nflow_simulate__(NFlow.internal.acausalExpand(d)));
assert_isequal(r.status, 'done');
v = r.scopes(1).series(:);
assert_isapprox(v(end), 10 * exp(-3), 5e-3);
%=============================================================================
