%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Hybrid ideal diode IdealDiode (Ron / Goff / Vknee): a leak conductance Goff
% when off, Ron in series with Vknee when on; the mode flip is a state event.
% Native physicalIsland kind idealDiode (one algebraic state s, zero-crossing
% surface s = 0).
%=============================================================================
d = ['{"version":"1.0.0","format":"nflow-2","name":"ID","blocks":[', ...
  '{"id":"V1","type":"ConstantVoltage","params":{"V":10}},', ...
  '{"id":"R1","type":"Resistor","params":{"R":100}},', ...
  '{"id":"D1","type":"IdealDiode","params":{"Ron":1e-3,"Goff":1e-6,"Vknee":0.7}},', ...
  '{"id":"G","type":"Ground","params":{}},', ...
  '{"id":"VS","type":"VoltageSensor","params":{}},', ...
  '{"id":"sc","type":"scope","inputs":1,"outputs":0,"params":{}}],', ...
  '"connections":[{"from":"VS","to":"sc","fromIndex":0,"toIndex":0}],', ...
  '"physicalWires":[{"pins":["V1.p","R1.p"]},{"pins":["R1.n","D1.p","VS.p"]},', ...
  '{"pins":["V1.n","D1.n","VS.n","G.p"]}],', ...
  '"solver":"dae","runtime":0.1,"sampleTime":0.02}'];
% Forward bias: the diode conducts, i = (10 - Vknee) / (R + Ron), and the
% voltage across it sits just above the knee: Vknee + Ron * i.
r = jsondecode(__nflow_simulate__(NFlow.internal.acausalExpand(d)));
assert_isequal(r.status, 'done');
i_fwd = (10 - 0.7) / (100 + 1e-3);
v_fwd = 0.7 + 1e-3 * i_fwd;
assert_isapprox(r.scopes(1).series(end), v_fwd, 1e-6);
% Reverse bias: only the leak Goff flows, so almost all of -10 V drops on the
% diode: v = -10 * (1 / Goff) / (R + 1 / Goff).
d2 = strrep(d, '"V":10', '"V":-10');
r = jsondecode(__nflow_simulate__(NFlow.internal.acausalExpand(d2)));
assert_isequal(r.status, 'done');
v_rev = -10 * (1 / 1e-6) / (100 + 1 / 1e-6);
assert_isapprox(r.scopes(1).series(end), v_rev, 1e-6);
% Knee at 0 V (default): a 1 V source through 1 ohm and Ron = 0.1 ohm gives
% i = 1 / 1.1 and a diode drop Ron * i.
d3 = strrep(strrep(strrep(d, '"V":10', '"V":1'), '"R":100', '"R":1'), '"Ron":1e-3,"Goff":1e-6,"Vknee":0.7', '"Ron":0.1,"Goff":1e-6');
r = jsondecode(__nflow_simulate__(NFlow.internal.acausalExpand(d3)));
assert_isequal(r.status, 'done');
assert_isapprox(r.scopes(1).series(end), 0.1 / 1.1, 1e-6);
% The ideal diode is also accepted by the fixed-step loop and the explicit solvers.
for solver = {'ode1', 'ode4', 'ode45'}
  r = jsondecode(__nflow_simulate__(NFlow.internal.acausalExpand(strrep(d, '"solver":"dae"', ['"solver":"', solver{1}, '"']))));
  assert_isequal(r.status, 'done');
  assert_isapprox(r.scopes(1).series(end), v_fwd, 1e-4);
end
