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
% repeatingSequenceStair: a periodic staircase source that emits
% one OutValues entry per sample and repeats from the start.
%=============================================================================
% OutValues = [10 20 30] over 11 samples: 10 20 30 10 20 30 10 20 30 10 20.
%=============================================================================
evalin('base', 'clear(''rss_out'')');
d = struct();
d.blocks = {...
  struct('id', 'r', 'type', 'repeatingSequenceStair', 'inputs', 0, 'outputs', 1, ...
    'params', struct('OutValues', [10 20 30])), ...
  struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'rss_out', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {struct('from', 'r', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1; d.duration = 1.0; d.solver = 'discrete'; d.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
y = evalin('base', 'rss_out'); y = y(:)';
assert_istrue(max(abs(y - [10 20 30 10 20 30 10 20 30 10 20])) < 1e-9);
evalin('base', 'clear(''rss_out'')');

%=============================================================================
% A single-entry sequence is a constant source.
%=============================================================================
evalin('base', 'clear(''rss_out'')');
d2 = d;
d2.blocks{1}.params.OutValues = [7];
r2 = jsondecode(__nflow_simulate__(jsonencode(d2)));
assert_isequal(r2.status, 'done');
y2 = evalin('base', 'rss_out'); y2 = y2(:)';
assert_istrue(max(abs(y2 - 7 * ones(1, 11))) < 1e-9);
evalin('base', 'clear(''rss_out'')');
%=============================================================================
