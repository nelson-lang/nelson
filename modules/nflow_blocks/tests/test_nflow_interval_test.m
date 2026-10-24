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
% intervalTest: output 1 when the input lies within
% [LowerLimit, UpperLimit] (closed by default), else 0.
%=============================================================================
% A ramp (u = t) fed through intervalTest with [0.25, 0.65]: the samples at
% t = 0.3,0.4,0.5,0.6 (u strictly inside) give 1, the rest 0. The bounds 0.25 /
% 0.65 are chosen off the sample grid to avoid floating-point boundary ties.
%=============================================================================
evalin('base', 'clear(''it_out'')');
d = struct();
d.blocks = {...
  struct('id', 'r', 'type', 'ramp', 'inputs', 0, 'outputs', 1, ...
    'params', struct('slope', 1)), ...
  struct('id', 'it', 'type', 'intervalTest', 'inputs', 1, 'outputs', 1, ...
    'params', struct('LowerLimit', 0.25, 'UpperLimit', 0.65, ...
    'IntervalClosedLeft', 1, 'IntervalClosedRight', 1)), ...
  struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'it_out', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {...
  struct('from', 'r', 'to', 'it', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'it', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1; d.duration = 1.0; d.solver = 'discrete'; d.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
y = evalin('base', 'it_out'); y = y(:)';
yexp = [0 0 0 1 1 1 1 0 0 0 0];
assert_isequal(numel(y), numel(yexp));
assert_istrue(max(abs(y - yexp)) < 1e-9);
evalin('base', 'clear(''it_out'')');

%=============================================================================
% Open interval (bounds excluded), tested off the sample grid to avoid
% floating-point boundary ties: [0.35, 0.55] exclusive selects only t=0.4,0.5.
%=============================================================================
evalin('base', 'clear(''it_out2'')');
d2 = d;
d2.blocks{2}.params = struct('LowerLimit', 0.35, 'UpperLimit', 0.55, ...
  'IntervalClosedLeft', 0, 'IntervalClosedRight', 0);
d2.blocks{3}.params.VariableName = 'it_out2';
r2 = jsondecode(__nflow_simulate__(jsonencode(d2)));
assert_isequal(r2.status, 'done');
y2 = evalin('base', 'it_out2'); y2 = y2(:)';
yexp2 = [0 0 0 0 1 1 0 0 0 0 0]; % only t = 0.4, 0.5 inside (0.35, 0.55)
assert_istrue(max(abs(y2 - yexp2)) < 1e-9);
evalin('base', 'clear(''it_out2'')');
%=============================================================================
