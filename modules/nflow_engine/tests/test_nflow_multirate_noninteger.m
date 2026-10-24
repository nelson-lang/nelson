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
% Commensurate non-integer sample times.
%
% A SampleTime that is not an integer multiple of the base step but is
% commensurate with it (a clean fraction) is accepted: the base step is refined
% to the GCD so every rate becomes integer, and output follows at the refined
% grid. An all-integer-multiple model is unchanged (no regression).
%=============================================================================

%=============================================================================
% Base 0.1, gain SampleTime 0.25 -> GCD 0.05 (factor 2). The gain (feedthrough,
% sample-and-hold) samples the ramp at t = 0,0.25,0.5,0.75,1.0 and
% holds between; the recorded output is on the refined 0.05 grid (21 points).
%   gain = 2*ramp(hit) = 0, 0.5, 1.0, 1.5, 2.0 held over 5 refined steps each.
%=============================================================================
evalin('base', 'clear(''mr_ni'')');
d = struct();
d.blocks = {...
  struct('id', 'r', 'type', 'ramp', 'inputs', 0, 'outputs', 1, ...
    'params', struct('slope', 1)), ...
  struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, ...
    'params', struct('Gain', 2, 'SampleTime', 0.25)), ...
  struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'mr_ni', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {...
  struct('from', 'r', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'g', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1; d.duration = 1.0; d.solver = 'discrete'; d.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
y = evalin('base', 'mr_ni'); y = y(:)';
yexp = [0 0 0 0 0, 0.5 0.5 0.5 0.5 0.5, 1.0 1.0 1.0 1.0 1.0, ...
        1.5 1.5 1.5 1.5 1.5, 2.0];
assert_isequal(numel(y), 21);
assert_istrue(max(abs(y - yexp)) < 1e-9);
evalin('base', 'clear(''mr_ni'')');

%=============================================================================
% Two commensurate sub-rates on the same base: 0.1 with a 0.25 and a 0.15 block
% -> GCD(0.1,0.25,0.15) = 0.05. Just assert it runs (no reject) and the grid is
% refined to 0.05 (21 points), i.e. both rates were made integer at once.
%=============================================================================
evalin('base', 'clear(''mr_ni2'')');
d2 = struct();
d2.blocks = {...
  struct('id', 'r', 'type', 'ramp', 'inputs', 0, 'outputs', 1, ...
    'params', struct('slope', 1)), ...
  struct('id', 'g1', 'type', 'gain', 'inputs', 1, 'outputs', 1, ...
    'params', struct('Gain', 1, 'SampleTime', 0.25)), ...
  struct('id', 'g2', 'type', 'gain', 'inputs', 1, 'outputs', 1, ...
    'params', struct('Gain', 1, 'SampleTime', 0.15)), ...
  struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'mr_ni2', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
d2.connections = {...
  struct('from', 'r', 'to', 'g1', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'g1', 'to', 'g2', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'g2', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
d2.sampleTime = 0.1; d2.duration = 1.0; d2.solver = 'discrete'; d2.variables = struct();
r2 = jsondecode(__nflow_simulate__(jsonencode(d2)));
assert_isequal(r2.status, 'done');
y2 = evalin('base', 'mr_ni2'); y2 = y2(:)';
assert_isequal(numel(y2), 21); % base refined to 0.05
evalin('base', 'clear(''mr_ni2'')');

%=============================================================================
% Regression: an all-integer-multiple model keeps its base (no refinement).
% Base 0.1, SampleTime 0.2 -> 11 points over [0,1] (NOT refined).
%=============================================================================
evalin('base', 'clear(''mr_ni3'')');
d3 = d;
d3.blocks{2}.params.SampleTime = 0.2;
d3.blocks{3}.params.VariableName = 'mr_ni3';
r3 = jsondecode(__nflow_simulate__(jsonencode(d3)));
assert_isequal(r3.status, 'done');
y3 = evalin('base', 'mr_ni3'); y3 = y3(:)';
assert_isequal(numel(y3), 11); % base unchanged at 0.1
evalin('base', 'clear(''mr_ni3'')');
%=============================================================================
