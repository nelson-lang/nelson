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
% risingEdge / fallingEdge: detect a zero crossing.
%   risingEdge  = 1 when previous <= 0 and current > 0.
%   fallingEdge = 1 when previous >= 0 and current < 0.
% Fed directly from a source (edge blocks sample their input in the OUTPUT
% phase, so a feedthrough input would be one step stale -- like `difference`).
%=============================================================================

%=============================================================================
% risingEdge on a rising ramp u = t (0, 0.1, 0.2, ...), InitialCondition = -1:
% at t=0, u=0 is not > 0 (no edge); at t=0.1, u=0.1 > 0 with previous u=0 <= 0,
% so risingEdge = 1 only at t=0.1.
%=============================================================================
evalin('base', 'clear(''re_out'')');
d = struct();
d.blocks = {...
  struct('id', 'r', 'type', 'ramp', 'inputs', 0, 'outputs', 1, ...
    'params', struct('slope', 1)), ...
  struct('id', 'e', 'type', 'risingEdge', 'inputs', 1, 'outputs', 1, ...
    'params', struct('InitialCondition', -1)), ...
  struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 're_out', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {...
  struct('from', 'r', 'to', 'e', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'e', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1; d.duration = 1.0; d.solver = 'discrete'; d.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
y = evalin('base', 're_out'); y = y(:)';
yexp = [0 1 0 0 0 0 0 0 0 0 0];
assert_isequal(numel(y), numel(yexp));
assert_istrue(max(abs(y - yexp)) < 1e-9);
evalin('base', 'clear(''re_out'')');

%=============================================================================
% fallingEdge on a falling ramp u = -t (0, -0.1, -0.2, ...), InitialCondition
% = 1: at t=0, u=0 is not < 0 (no edge); at t=0.1, u=-0.1 < 0 with previous
% u=0 >= 0, so fallingEdge = 1 only at t=0.1.
%=============================================================================
evalin('base', 'clear(''fe_out'')');
d2 = d;
d2.blocks{1}.params = struct('slope', -1);
d2.blocks{2} = struct('id', 'e', 'type', 'fallingEdge', 'inputs', 1, 'outputs', 1, ...
  'params', struct('InitialCondition', 1));
d2.blocks{3}.params.VariableName = 'fe_out';
r2 = jsondecode(__nflow_simulate__(jsonencode(d2)));
assert_isequal(r2.status, 'done');
y2 = evalin('base', 'fe_out'); y2 = y2(:)';
yexp2 = [0 1 0 0 0 0 0 0 0 0 0];
assert_istrue(max(abs(y2 - yexp2)) < 1e-9);
evalin('base', 'clear(''fe_out'')');
%=============================================================================
