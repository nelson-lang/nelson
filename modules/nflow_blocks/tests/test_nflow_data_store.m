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
% Data Store Memory / Write / Read: a named scalar memory shared
% without a wire. Read observes the previous step's Write (one-step latency).
%=============================================================================
% Memory 'M' starts at 0. A ramp (u = t) is written to 'M' every step; a Read of
% 'M' feeds toWorkspace. Read(0) = initial 0; Read(k) = ramp(k-1) for k >= 1, so
% the recorded output is the ramp delayed by one step (0, 0, 0.1, 0.2, ..., 0.9).
%=============================================================================
evalin('base', 'clear(''ds_out'')');
d = struct();
d.blocks = {...
  struct('id', 'mem', 'type', 'dataStoreMemory', 'inputs', 0, 'outputs', 0, ...
    'params', struct('DataStoreName', 'M', 'InitialValue', 0)), ...
  struct('id', 'r', 'type', 'ramp', 'inputs', 0, 'outputs', 1, ...
    'params', struct('slope', 1)), ...
  struct('id', 'wr', 'type', 'dataStoreWrite', 'inputs', 1, 'outputs', 0, ...
    'params', struct('DataStoreName', 'M')), ...
  struct('id', 'rd', 'type', 'dataStoreRead', 'inputs', 0, 'outputs', 1, ...
    'params', struct('DataStoreName', 'M')), ...
  struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'ds_out', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {...
  struct('from', 'r', 'to', 'wr', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'rd', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1; d.duration = 1.0; d.solver = 'discrete'; d.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
y = evalin('base', 'ds_out'); y = y(:)';
% Every write of a store runs before every read of it, so a read answers with
% the value written this step - the ramp itself, not the ramp of the step
% before.
yexp = [0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0];
assert_isequal(numel(y), numel(yexp));
assert_istrue(max(abs(y - yexp)) < 1e-9);
evalin('base', 'clear(''ds_out'')');

%=============================================================================
% A lone Read (no Memory, no Write) of an undeclared store reads 0.
%=============================================================================
evalin('base', 'clear(''ds_out'')');
d2 = struct();
d2.blocks = {...
  struct('id', 'rd', 'type', 'dataStoreRead', 'inputs', 0, 'outputs', 1, ...
    'params', struct('DataStoreName', 'Unused')), ...
  struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'ds_out', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
d2.connections = {struct('from', 'rd', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
d2.sampleTime = 0.1; d2.duration = 0.3; d2.solver = 'discrete'; d2.variables = struct();
r2 = jsondecode(__nflow_simulate__(jsonencode(d2)));
assert_isequal(r2.status, 'done');
y2 = evalin('base', 'ds_out'); y2 = y2(:)';
assert_istrue(max(abs(y2)) < 1e-9);
evalin('base', 'clear(''ds_out'')');
%=============================================================================
