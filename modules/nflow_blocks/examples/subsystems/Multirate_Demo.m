%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Multi-rate demo: a discrete block running slower than the base step.
%
% A block whose 'SampleTime' parameter is a multiple of the model's base step
% updates only on its own sample hits and holds its output between them
% (zero-order hold). Here a unit delay is driven by a unit-slope ramp: at the
% base rate its output tracks the ramp every step; at four times the base step it
% samples every fourth step, producing a coarse staircase. A block without a
% SampleTime keeps the single-rate behaviour, so this is a pure opt-in extension.
%=============================================================================
base = 0.1; % base step
duration = 2.0;
mk = @(ts) struct('blocks', {{...
  struct('id', 'r', 'type', 'ramp', 'inputs', 0, 'outputs', 1, ...
    'params', struct('slope', 1)), ...
  struct('id', 'ud', 'type', 'unitDelay', 'inputs', 1, 'outputs', 1, ...
    'params', struct('InitialCondition', 0, 'SampleTime', ts)), ...
  struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'y', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))}}, ...
  'connections', {{...
    struct('from', 'r', 'to', 'ud', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'ud', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)}}, ...
  'sampleTime', base, 'duration', duration, 'solver', 'discrete', ...
  'variables', struct());
%=============================================================================
% Base rate: the unit delay tracks the ramp every step.
jsondecode(__nflow_simulate__(jsonencode(mk(0))));
yBase = evalin('base', 'y'); yBase = yBase(:)';
%=============================================================================
% Four times the base step: a coarse staircase held four samples at a time.
jsondecode(__nflow_simulate__(jsonencode(mk(4 * base))));
ySlow = evalin('base', 'y'); ySlow = ySlow(:)';
evalin('base', 'clear(''y'')');
%=============================================================================
t = (0:base:duration);
disp('   t     base    Ts=4*base');
disp([t(:), yBase(:), ySlow(:)]);
%=============================================================================
