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
% counterFreeRunning / counterLimited: free-running and wrapping
% discrete up-counters. No input; each emits the current count, then increments
% at the end of the step. The first sample is 0.
%=============================================================================
% counterLimited with UpperLimit = 3 over 11 samples: 0 1 2 3 0 1 2 3 0 1 2.
%=============================================================================
evalin('base', 'clear(''cl_out'')');
d = struct();
d.blocks = {...
  struct('id', 'c', 'type', 'counterLimited', 'inputs', 0, 'outputs', 1, ...
    'params', struct('UpperLimit', 3)), ...
  struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'cl_out', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {struct('from', 'c', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1; d.duration = 1.0; d.solver = 'discrete'; d.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
y = evalin('base', 'cl_out'); y = y(:)';
assert_istrue(max(abs(y - [0 1 2 3 0 1 2 3 0 1 2])) < 1e-9);
evalin('base', 'clear(''cl_out'')');

%=============================================================================
% counterFreeRunning with NumBits = 2 (modulus 4) over 11 samples wraps like a
% limited counter with limit 3: 0 1 2 3 0 1 2 3 0 1 2.
%=============================================================================
evalin('base', 'clear(''cf_out'')');
d2 = struct();
d2.blocks = {...
  struct('id', 'c', 'type', 'counterFreeRunning', 'inputs', 0, 'outputs', 1, ...
    'params', struct('NumBits', 2)), ...
  struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'cf_out', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
d2.connections = {struct('from', 'c', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
d2.sampleTime = 0.1; d2.duration = 1.0; d2.solver = 'discrete'; d2.variables = struct();
r2 = jsondecode(__nflow_simulate__(jsonencode(d2)));
assert_isequal(r2.status, 'done');
y2 = evalin('base', 'cf_out'); y2 = y2(:)';
assert_istrue(max(abs(y2 - [0 1 2 3 0 1 2 3 0 1 2])) < 1e-9);
evalin('base', 'clear(''cf_out'')');
%=============================================================================
