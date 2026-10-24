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
% stopSimulation: ends the run at the end of the step where the
% input becomes nonzero (via SimCtx::stopRequested, honored by both loops).
%=============================================================================
% A step (0 before StepTime=0.45, then 1) drives stopSimulation and is also
% recorded. The run should stop at t=0.5 (the first sample where step == 1), so
% the recorded output is shorter than the full 11 samples and ends with 1.
%=============================================================================
evalin('base', 'clear(''ss_out'')');
d = struct();
d.blocks = {...
  struct('id', 's', 'type', 'step', 'inputs', 0, 'outputs', 1, ...
    'params', struct('Time', 0.45)), ...
  struct('id', 'stop', 'type', 'stopSimulation', 'inputs', 1, 'outputs', 0, ...
    'params', struct()), ...
  struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'ss_out', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {...
  struct('from', 's', 'to', 'stop', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 's', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1; d.duration = 1.0; d.solver = 'discrete'; d.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
y = evalin('base', 'ss_out'); y = y(:)';
% Stopped early: fewer than the full 11 samples, and the last recorded value is
% the nonzero step that triggered the stop.
assert_istrue(numel(y) < 11);
assert_istrue(numel(y) >= 5);
assert_istrue(abs(y(end) - 1.0) < 1e-9);
% Everything before the trigger is 0 (step below StepTime).
assert_istrue(max(abs(y(1:end - 1))) < 1e-9);
evalin('base', 'clear(''ss_out'')');
%=============================================================================
