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
% initialCondition: outputs InitialValue at t=0, then passes the
% input through for t > 0.
%=============================================================================
% A constant input (5) through IC (InitialValue=99): the first sample is 99, the
% rest are 5.
%=============================================================================
evalin('base', 'clear(''ic_out'')');
d = struct();
d.blocks = {...
  struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, ...
    'params', struct('Value', 5)), ...
  struct('id', 'ic', 'type', 'initialCondition', 'inputs', 1, 'outputs', 1, ...
    'params', struct('InitialValue', 99)), ...
  struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'ic_out', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {...
  struct('from', 'c', 'to', 'ic', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ic', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1; d.duration = 1.0; d.solver = 'discrete'; d.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
y = evalin('base', 'ic_out'); y = y(:)';
yexp = [99 5 5 5 5 5 5 5 5 5 5];
assert_isequal(numel(y), numel(yexp));
assert_istrue(max(abs(y - yexp)) < 1e-9);
evalin('base', 'clear(''ic_out'')');

%=============================================================================
% A ramp input through IC: first sample = InitialValue, then the ramp passes
% through (u = t).
%=============================================================================
evalin('base', 'clear(''ic_out2'')');
d2 = d;
d2.blocks{1} = struct('id', 'c', 'type', 'ramp', 'inputs', 0, 'outputs', 1, ...
  'params', struct('slope', 1));
d2.blocks{2}.params = struct('InitialValue', -7);
d2.blocks{3}.params.VariableName = 'ic_out2';
r2 = jsondecode(__nflow_simulate__(jsonencode(d2)));
assert_isequal(r2.status, 'done');
y2 = evalin('base', 'ic_out2'); y2 = y2(:)';
yexp2 = [-7 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0];
assert_istrue(max(abs(y2 - yexp2)) < 1e-9);
evalin('base', 'clear(''ic_out2'')');
%=============================================================================
