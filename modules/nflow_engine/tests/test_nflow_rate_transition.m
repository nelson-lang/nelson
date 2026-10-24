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
% B5: minimal multi-rate. A rateTransition block resamples its input at its own
% OutPortSampleTime (0.3), holding between ticks, while the base step is 0.1.
% Feeding a clock (t), the zero-order-hold output stays constant for three base
% steps and steps up every 0.3 s: 0 0 0 0 0.3 0.3 0.3 0.6 0.6 0.6 0.9.
%=============================================================================
evalin('base', 'clear(''rt_out'')');

d = struct();
d.blocks = {...
  struct('id', 'clk', 'type', 'clock', 'inputs', 0, 'outputs', 1, 'params', struct()), ...
  struct('id', 'rt', 'type', 'rateTransition', 'inputs', 1, 'outputs', 1, ...
    'params', struct('OutPortSampleTime', 0.3, 'InitialCondition', 0)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'rt_out', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {...
  struct('from', 'clk', 'to', 'rt', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'rt', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1; d.duration = 1.0; d.variables = struct();

r = jsondecode(__nflow_simulate__(jsonencode(d)));
asserts.isequal(r.status, 'done');
v = evalin('base', 'rt_out');
v = v(:)';
asserts.isequal(numel(v), 11);
% The tick answers the input it has just sampled, so the value appears at the
% tick itself rather than one output period later.
expected = [0 0 0 0.3 0.3 0.3 0.6 0.6 0.6 0.9 0.9];
asserts.isapprox(v, expected, 0, 1e-9);

% Sanity: an OutPortSampleTime <= the base step samples every step, so the
% output IS the input - the block is a wire at that rate.
evalin('base', 'clear(''rt_fast'')');
d.blocks{2}.params.OutPortSampleTime = 0.1;
d.blocks{3}.params.VariableName = 'rt_fast';
r2 = jsondecode(__nflow_simulate__(jsonencode(d)));
asserts.isequal(r2.status, 'done');
vf = evalin('base', 'rt_fast'); vf = vf(:)';
asserts.isequal(numel(vf), 11);
% The clock itself: 0, 0.1, 0.2, ... 1.0.
asserts.isapprox(vf(1), 0.0, 0, 1e-9);
asserts.isapprox(vf(2), 0.1, 0, 1e-9);
asserts.isapprox(vf(3), 0.2, 0, 1e-9);
asserts.isapprox(vf(end), 1.0, 0, 1e-9);

evalin('base', 'clear(''rt_out'')');
evalin('base', 'clear(''rt_fast'')');
%=============================================================================
