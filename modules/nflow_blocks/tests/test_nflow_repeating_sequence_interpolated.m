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
% repeatingSequenceInterpolated: a periodic, piecewise-linear
% source. The (TimeValues, OutValues) table defines one period (period = last
% TimeValues); the output linearly interpolates it and repeats.
%=============================================================================
% TimeValues = [0 0.5 1], OutValues = [0 1 0] -> a triangle wave of period 1:
% rises 0->1 over [0,0.5], falls 1->0 over [0.5,1]. Sampled at 0.1 over [0,1]:
% 0 0.2 0.4 0.6 0.8 1.0 0.8 0.6 0.4 0.2 0.0.
%=============================================================================
evalin('base', 'clear(''rsi_out'')');
d = struct();
d.blocks = {...
  struct('id', 'r', 'type', 'repeatingSequenceInterpolated', 'inputs', 0, 'outputs', 1, ...
    'params', struct('TimeValues', [0 0.5 1], 'OutValues', [0 1 0])), ...
  struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'rsi_out', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {struct('from', 'r', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1; d.duration = 1.0; d.solver = 'discrete'; d.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
y = evalin('base', 'rsi_out'); y = y(:)';
yexp = [0 0.2 0.4 0.6 0.8 1.0 0.8 0.6 0.4 0.2 0.0];
assert_isequal(numel(y), numel(yexp));
assert_istrue(max(abs(y - yexp)) < 1e-9);
evalin('base', 'clear(''rsi_out'')');
%=============================================================================
