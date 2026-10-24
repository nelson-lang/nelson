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
% hitCrossing: outputs 1 on the step where the input crosses
% HitCrossingOffset in the configured direction, else 0. Stateful; the first
% step is primed and never fires.
%=============================================================================
% Drive hitCrossing directly from a step source (a direct OUTPUT-phase source,
% so the input is read fresh; feeding it through an ALGEBRAIC feedthrough block
% would make the input one step stale). step(Time = 0.45) is 0 for t < 0.45 and
% 1 afterwards; relative to HitCrossingOffset = 0.5 that is -0.5 then +0.5, so a
% single rising crossing occurs at t = 0.5.
%=============================================================================
evalin('base', 'clear(''hc_out'')');
d = struct();
d.blocks = {...
  struct('id', 's', 'type', 'step', 'inputs', 0, 'outputs', 1, ...
    'params', struct('Time', 0.45)), ...
  struct('id', 'hc', 'type', 'hitCrossing', 'inputs', 1, 'outputs', 1, ...
    'params', struct('HitCrossingOffset', 0.5, 'HitCrossingDirection', 'rising')), ...
  struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'hc_out', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {...
  struct('from', 's', 'to', 'hc', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'hc', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1; d.duration = 1.0; d.solver = 'discrete'; d.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
y = evalin('base', 'hc_out'); y = y(:)';
% Exactly one rising crossing, landing on t = 0.5 (the 6th sample).
assert_isequal(sum(y), 1);
assert_istrue(abs(y(6) - 1.0) < 1e-9);
evalin('base', 'clear(''hc_out'')');

%=============================================================================
% Same step but direction "falling": the input only ever rises, so no falling
% crossing is detected and the output is 0 at every sample.
%=============================================================================
evalin('base', 'clear(''hc_out'')');
d2 = d;
d2.blocks{2}.params.HitCrossingDirection = 'falling';
r2 = jsondecode(__nflow_simulate__(jsonencode(d2)));
assert_isequal(r2.status, 'done');
y2 = evalin('base', 'hc_out'); y2 = y2(:)';
assert_isequal(sum(y2), 0);
evalin('base', 'clear(''hc_out'')');
%=============================================================================
