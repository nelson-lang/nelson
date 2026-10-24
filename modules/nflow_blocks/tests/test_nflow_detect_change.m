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
% detectChange: output 1 on any step where the input differs
% from its value at the previous step, else 0. InitialCondition seeds the value
% "before" the first step.
%=============================================================================
% A ramp (u = t) changes every step, so detectChange is 1 at every step except
% the first, where u(0) = 0 equals the default InitialCondition 0.
%=============================================================================
evalin('base', 'clear(''dc_out'')');
d = struct();
d.blocks = {...
  struct('id', 'r', 'type', 'ramp', 'inputs', 0, 'outputs', 1, ...
    'params', struct('slope', 1)), ...
  struct('id', 'dc', 'type', 'detectChange', 'inputs', 1, 'outputs', 1, ...
    'params', struct('InitialCondition', 0)), ...
  struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'dc_out', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {...
  struct('from', 'r', 'to', 'dc', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'dc', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1; d.duration = 1.0; d.solver = 'discrete'; d.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
y = evalin('base', 'dc_out'); y = y(:)';
yexp = [0 1 1 1 1 1 1 1 1 1 1];
assert_isequal(numel(y), numel(yexp));
assert_istrue(max(abs(y - yexp)) < 1e-9);
evalin('base', 'clear(''dc_out'')');

%=============================================================================
% A constant input (5) with InitialCondition 0: a single change at the first
% step (5 != 0), then no change.
%=============================================================================
evalin('base', 'clear(''dc_out2'')');
d2 = d;
d2.blocks{1} = struct('id', 'r', 'type', 'constant', 'inputs', 0, 'outputs', 1, ...
  'params', struct('Value', 5));
d2.blocks{3}.params.VariableName = 'dc_out2';
r2 = jsondecode(__nflow_simulate__(jsonencode(d2)));
assert_isequal(r2.status, 'done');
y2 = evalin('base', 'dc_out2'); y2 = y2(:)';
yexp2 = [1 0 0 0 0 0 0 0 0 0 0];
assert_istrue(max(abs(y2 - yexp2)) < 1e-9);
evalin('base', 'clear(''dc_out2'')');
%=============================================================================
