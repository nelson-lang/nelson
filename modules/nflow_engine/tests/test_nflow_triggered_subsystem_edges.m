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
% <--WEIGHT 64-->
% <--TIMEOUT 600-->
%=============================================================================
% Regression: a falling edge was read as "the control became strictly
% NEGATIVE". A gate signal that swings between 0 and 1 - which is what a gate
% signal normally does - never goes below zero, so a falling-edge trigger
% never fired at all and an either-edge trigger only ever saw its rising
% halves. Rising had the mirror hole: a negative control returning to zero was
% not an edge. The located-edge path in the same file already used the right
% rule; the sample-point gate and the emitted C/Rust now agree with it.
%=============================================================================
% Control = a 0/1 pulse of period 0.4 sampled every 0.1:
%   t    0   .1  .2  .3  .4  .5  .6  .7  .8  .9  1.0
%   ctl  1   1   0   0   1   1   0   0   1   1   0
% falling edges at t = 0.2, 0.6, 1.0 ; rising edges at t = 0.4, 0.8.
% The body samples the clock through a gain of 2, so a fire at time T leaves
% 2*T on the output until the next one.
%=============================================================================
inner = struct();
inner.blocks = {...
  struct('id', 'in1', 'type', 'labelSource', 'inputs', 0, 'outputs', 1, ...
    'params', struct('label', 'in1')), ...
  struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, ...
    'params', struct('Gain', 2)), ...
  struct('id', 'out1', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('label', 'out1'))};
inner.connections = {...
  struct('from', 'in1', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'g', 'to', 'out1', 'fromIndex', 0, 'toIndex', 0)};
inner.externalInputs = {struct('id', 'in1', 'name', 'in1')};
inner.externalOutputs = {struct('id', 'out1', 'name', 'out1')};

d = struct();
d.blocks = {...
  struct('id', 'clk', 'type', 'clock', 'inputs', 0, 'outputs', 1, 'params', struct()), ...
  struct('id', 'ctl', 'type', 'pulse', 'inputs', 0, 'outputs', 1, ...
    'params', struct('Amplitude', 1, 'Period', 0.4, 'Width', 50, ...
      'StartTime', 0, 'Offset', 0)), ...
  struct('id', 'sub', 'type', 'subsystem', 'inputs', 2, 'outputs', 1, ...
    'params', struct('subsystem', inner, ...
      'controlPort', struct('kind', 'trigger', 'triggerType', 'falling'))), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'nfte_y', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {...
  struct('from', 'clk', 'to', 'sub', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ctl', 'to', 'sub', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 'sub', 'to', 'y', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'sub', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1;
d.duration = 1.0;
d.variables = struct();
%=============================================================================
% Falling: every 1 -> 0 fall fires. It used to hold 0 for the whole run.
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
y = evalin('base', 'nfte_y');
expected = [0, 0, 0.4, 0.4, 0.4, 0.4, 1.2, 1.2, 1.2, 1.2, 2.0];
assert_isequal(numel(y), numel(expected));
assert_istrue(max(abs(y(:)' - expected)) < 1e-9);
evalin('base', 'clear(''nfte_y'')');
%=============================================================================
% Either: both halves. It used to fire on the rising edges only.
de = d;
de.blocks{3}.params.controlPort = struct('kind', 'trigger', 'triggerType', 'either');
re = jsondecode(__nflow_simulate__(jsonencode(de)));
assert_isequal(re.status, 'done');
ye = evalin('base', 'nfte_y');
expectedE = [0, 0, 0.4, 0.4, 0.8, 0.8, 1.2, 1.2, 1.6, 1.6, 2.0];
assert_istrue(max(abs(ye(:)' - expectedE)) < 1e-9);
evalin('base', 'clear(''nfte_y'')');
%=============================================================================
% Rising is unchanged by any of this: 0 -> 1 still fires, and nothing else.
dr = d;
dr.blocks{3}.params.controlPort = struct('kind', 'trigger', 'triggerType', 'rising');
rr = jsondecode(__nflow_simulate__(jsonencode(dr)));
assert_isequal(rr.status, 'done');
yr = evalin('base', 'nfte_y');
expectedR = [0, 0, 0, 0, 0.8, 0.8, 0.8, 0.8, 1.6, 1.6, 1.6];
assert_istrue(max(abs(yr(:)' - expectedR)) < 1e-9);
evalin('base', 'clear(''nfte_y'')');
%=============================================================================
% A control that lands EXACTLY on zero: t - 0.5 reaches 0 at t = 0.5 coming
% from below. That is one physical crossing and must fire once - reaching zero
% is the edge, leaving it is not. It used to be latched as a plain zero, which
% made the next step (0 -> 0.1) look like a second rising edge.
dz = d;
dz.blocks{2} = struct('id', 'ctl', 'type', 'sum', 'inputs', 2, 'outputs', 1, ...
  'params', struct('Inputs', '++'));
dz.blocks{end + 1} = struct('id', 'k', 'type', 'constant', 'inputs', 0, ...
  'outputs', 1, 'params', struct('Value', -0.5));
dz.blocks{3}.params.controlPort = struct('kind', 'trigger', 'triggerType', 'either');
dz.connections{end + 1} = struct('from', 'clk', 'to', 'ctl', ...
  'fromIndex', 0, 'toIndex', 0);
dz.connections{end + 1} = struct('from', 'k', 'to', 'ctl', ...
  'fromIndex', 0, 'toIndex', 1);
rz = jsondecode(__nflow_simulate__(jsonencode(dz)));
assert_isequal(rz.status, 'done');
yz = evalin('base', 'nfte_y');
expectedZ = [0, 0, 0, 0, 0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0];
assert_istrue(max(abs(yz(:)' - expectedZ)) < 1e-9);
evalin('base', 'clear(''nfte_y'')');
%=============================================================================
% The emitted code carries the same edge rule as the simulator.
if __nflow_codegen_toolchain__('C')
  nflow_codegen_check_numeric(d, 'nfte_num', 'C', {'y'}, {'nfte_y'}, 5e-6, 1e-9);
  if __nflow_codegen_toolchain__('rust')
    nflow_codegen_check_numeric(d, 'nfte_num', 'rust', {'y'}, {'nfte_y'}, 5e-6, 1e-9);
  end
end
%=============================================================================
