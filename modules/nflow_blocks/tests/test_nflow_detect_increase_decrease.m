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
% detectIncrease / detectDecrease: output 1 on any step where
% the input is strictly greater (resp. strictly less) than its value at the
% previous step, else 0. InitialCondition seeds the value "before" step 0.
%=============================================================================
% A rising ramp (u = t): detectIncrease is 0 at the first step (u(0) = 0 equals
% the default InitialCondition 0) then 1 every step; detectDecrease stays 0.
%=============================================================================
evalin('base', 'clear(''di_out'')');
evalin('base', 'clear(''dd_out'')');
d = struct();
d.blocks = {...
  struct('id', 'r', 'type', 'ramp', 'inputs', 0, 'outputs', 1, ...
    'params', struct('slope', 1)), ...
  struct('id', 'di', 'type', 'detectIncrease', 'inputs', 1, 'outputs', 1, ...
    'params', struct('InitialCondition', 0)), ...
  struct('id', 'dd', 'type', 'detectDecrease', 'inputs', 1, 'outputs', 1, ...
    'params', struct('InitialCondition', 0)), ...
  struct('id', 'wi', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'di_out', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf')), ...
  struct('id', 'wd', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'dd_out', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {...
  struct('from', 'r', 'to', 'di', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'r', 'to', 'dd', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'di', 'to', 'wi', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'dd', 'to', 'wd', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1; d.duration = 1.0; d.solver = 'discrete'; d.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
yi = evalin('base', 'di_out'); yi = yi(:)';
yd = evalin('base', 'dd_out'); yd = yd(:)';
assert_istrue(max(abs(yi - [0 1 1 1 1 1 1 1 1 1 1])) < 1e-9);
assert_istrue(max(abs(yd - zeros(1, 11))) < 1e-9);
evalin('base', 'clear(''di_out'')');
evalin('base', 'clear(''dd_out'')');

%=============================================================================
% A falling ramp (u = -t): the mirror case. detectDecrease is 0 at the first
% step then 1 every step; detectIncrease stays 0.
%=============================================================================
evalin('base', 'clear(''di_out'')');
evalin('base', 'clear(''dd_out'')');
d2 = d;
d2.blocks{1}.params.slope = -1;
r2 = jsondecode(__nflow_simulate__(jsonencode(d2)));
assert_isequal(r2.status, 'done');
yi2 = evalin('base', 'di_out'); yi2 = yi2(:)';
yd2 = evalin('base', 'dd_out'); yd2 = yd2(:)';
assert_istrue(max(abs(yd2 - [0 1 1 1 1 1 1 1 1 1 1])) < 1e-9);
assert_istrue(max(abs(yi2 - zeros(1, 11))) < 1e-9);
evalin('base', 'clear(''di_out'')');
evalin('base', 'clear(''dd_out'')');
%=============================================================================
