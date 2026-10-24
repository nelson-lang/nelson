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
% intervalTestDynamic: output 1 when lo <= u <= up, with the
% bounds taken from input signals (port 0 = lo, port 1 = u, port 2 = up).
%=============================================================================
% Bounds lo = 1, up = 3 (constants); u = ramp (t over [0,1] with sampleTime 0.1
% gives 0,0.1,...,1.0). The value is never >= 1 until t = 1.0, so the test is 1
% only at the last sample (u = 1.0 == lo, closed left) and stays 0 before.
%=============================================================================
evalin('base', 'clear(''itd_out'')');
d = struct();
d.blocks = {...
  struct('id', 'lo', 'type', 'constant', 'inputs', 0, 'outputs', 1, ...
    'params', struct('Value', 1)), ...
  struct('id', 'u', 'type', 'ramp', 'inputs', 0, 'outputs', 1, ...
    'params', struct('slope', 1)), ...
  struct('id', 'up', 'type', 'constant', 'inputs', 0, 'outputs', 1, ...
    'params', struct('Value', 3)), ...
  struct('id', 'it', 'type', 'intervalTestDynamic', 'inputs', 3, 'outputs', 1, ...
    'params', struct('IntervalClosedLeft', 1, 'IntervalClosedRight', 1)), ...
  struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'itd_out', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {...
  struct('from', 'lo', 'to', 'it', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'u', 'to', 'it', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 'up', 'to', 'it', 'fromIndex', 0, 'toIndex', 2), ...
  struct('from', 'it', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1; d.duration = 1.0; d.solver = 'discrete'; d.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
y = evalin('base', 'itd_out'); y = y(:)';
% u reaches 1.0 (== lo) only at the final sample: 1 there, 0 everywhere before.
yexp = [0 0 0 0 0 0 0 0 0 0 1];
assert_isequal(numel(y), numel(yexp));
assert_istrue(max(abs(y - yexp)) < 1e-9);
evalin('base', 'clear(''itd_out'')');

%=============================================================================
% Open-left interval (IntervalClosedLeft = 0): u = 1.0 == lo now fails, so the
% output is 0 at every sample.
%=============================================================================
evalin('base', 'clear(''itd_out'')');
d2 = d;
d2.blocks{4}.params.IntervalClosedLeft = 0;
r2 = jsondecode(__nflow_simulate__(jsonencode(d2)));
assert_isequal(r2.status, 'done');
y2 = evalin('base', 'itd_out'); y2 = y2(:)';
assert_istrue(max(abs(y2 - zeros(1, 11))) < 1e-9);
evalin('base', 'clear(''itd_out'')');
%=============================================================================
