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
% A2.2: a limited integrator under a global (ode4) solver: the state engages
% its saturation limit via a zero-crossing and is held there, so the output
% clamps at the limit with no overshoot. Input u = 2, bounds [-1, 1], x(0) = 0:
% x' = 2 until x reaches 1 at t = 0.5, then held at 1.
%=============================================================================
evalin('base', 'clear(''sat_traj'')');
d = struct();
d.blocks = {...
  struct('id', 'u', 'type', 'constant', 'inputs', 0, 'outputs', 1, ...
    'params', struct('Value', 2)), ...
  struct('id', 'x', 'type', 'integrator', 'inputs', 1, 'outputs', 1, ...
    'params', struct('InitialCondition', 0, ...
    'LowerSaturationLimit', -1, 'UpperSaturationLimit', 1)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'sat_traj', 'SaveFormat', 'Structure With Time', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {...
  struct('from', 'u', 'to', 'x', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'x', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.01; d.duration = 1.0; d.variables = struct(); d.solver = 'ode4';
r = jsondecode(__nflow_simulate__(jsonencode(d)));
asserts.isequal(r.status, 'done');
sat_traj = evalin('base', 'sat_traj');
vals = sat_traj.signals.values(:);
% never exceeds the upper limit (no overshoot at the clamp)
asserts.istrue(max(vals) <= 1.0 + 1e-6);
% settles exactly on the limit
asserts.isapprox(vals(end), 1.0, 0, 1e-6);
% and actually reached it (the integration was not frozen early)
asserts.istrue(vals(end) > 0.99);
evalin('base', 'clear(''sat_traj'')');
%=============================================================================
