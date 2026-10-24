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
% Blocks exposed in the palette: pulse (Pulse Generator) and hysteresis
% (Relay, two-threshold latching switch).
%=============================================================================
% --- pulse: Amplitude 2, Period 1, Width 50% -> +2 for the first half period ---
tvec = (0:40) * 0.05;
evalin('base', 'clear(''p_out'')');
d = struct();
d.blocks = {...
  struct('id', 'p', 'type', 'pulse', 'inputs', 0, 'outputs', 1, ...
    'params', struct('Amplitude', 2, 'Period', 1, 'Width', 50, 'StartTime', 0, 'Offset', 0)), ...
  struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'p_out', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {struct('from', 'p', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.05; d.duration = 2.0; d.solver = 'discrete'; d.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
y = evalin('base', 'p_out'); y = y(:)';
assert_isequal(numel(y), numel(tvec));
pexp = 2 * double(mod(tvec, 1) < 0.5);
assert_istrue(max(abs(y - pexp)) < 1e-9);
evalin('base', 'clear(''p_out'')');

% --- hysteresis (Relay): sine amp 2 latches between yLow=0 and yHigh=1 ---
evalin('base', 'clear(''r_out'')');
d2 = struct();
d2.blocks = {...
  struct('id', 's', 'type', 'sine', 'inputs', 0, 'outputs', 1, ...
    'params', struct('Amplitude', 2, 'Frequency', 1)), ...
  struct('id', 'r', 'type', 'hysteresis', 'inputs', 1, 'outputs', 1, ...
    'params', struct('uHigh', 1, 'uLow', -1, 'yHigh', 1, 'yLow', 0)), ...
  struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'r_out', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
d2.connections = {struct('from', 's', 'to', 'r', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'r', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
d2.sampleTime = 0.02; d2.duration = 2.0; d2.solver = 'discrete'; d2.variables = struct();
r2 = jsondecode(__nflow_simulate__(jsonencode(d2)));
assert_isequal(r2.status, 'done');
yr = evalin('base', 'r_out'); yr = yr(:)';
% Expected latching: the relay is direct feedthrough, so it answers the input of
% the SAME step and only holds the latched branch inside the dead band. It used
% to emit the branch committed one step earlier, which delayed every switch by a
% step and disagreed with the C/Rust the block emits.
u = 2 * sin(2 * pi * (0:100) * 0.02);
s = 0; exp2 = zeros(1, numel(u));
for k = 1:numel(u)
  if u(k) >= 1
    s = 1;
  elseif u(k) <= -1
    s = 0;
  end
  exp2(k) = s;
end
assert_isequal(numel(yr), numel(exp2));
assert_istrue(max(abs(yr - exp2)) < 1e-9);
assert_isequal(max(yr), 1);
assert_isequal(min(yr), 0);
evalin('base', 'clear(''r_out'')');

% --- palette/catalog exposure: both types resolve through add_block ---
bdclose('all');
new_system('mpr');
p = add_block('nflow/source/pulse', 'mpr/P');
assert_isequal(p, 'mpr/P');
h = add_block('nflow/nonlinear/hysteresis', 'mpr/R');
assert_isequal(h, 'mpr/R');
assert_isequal(get_param('mpr/P', 'BlockType'), 'pulse');
assert_isequal(get_param('mpr/R', 'BlockType'), 'hysteresis');
bdclose('all');
%=============================================================================
