%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% For-each subsystem demo.
%
% A subsystem with params.forEach runs its body numIterations times per step.
% A partitioned input of outer width numIterations * w_in feeds iteration i its
% i-th w_in slice; an unpartitioned input is broadcast whole to every iteration;
% each inner output of width w_out is concatenated into an outer output of width
% numIterations * w_out. This applies one reusable sub-diagram element-wise
% across a vector / channel bank without hand-replicating blocks.
%
%   params.forEach = struct('numIterations', N, 'partition', {PORTS})
%
% NOTE the 'partition' value must be a CELL of the outer input-port indices
% (0-based), e.g. {0} or {0, 2} -- jsonencode of a length-1 numeric array is a
% bare number, which would not be seen as the JSON array the engine expects.
%
% The body may be stateless (gains, math, saturations) or carry per-iteration
% state, discrete (a unit delay / discrete filter) or continuous (an integrator
% / transfer function integrated by the solver): each iteration keeps its own
% independent history / integrates its own channel.
%=============================================================================

% Body: one scalar input, scaled by 3, to one scalar output.
inner = struct();
inner.blocks = {...
  struct('id', 'in1', 'type', 'labelSource', 'inputs', 0, 'outputs', 1, ...
    'params', struct('label', 'in1')), ...
  struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, ...
    'params', struct('Gain', 3)), ...
  struct('id', 'out1', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('label', 'out1'))};
inner.connections = {...
  struct('from', 'in1', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'g', 'to', 'out1', 'fromIndex', 0, 'toIndex', 0)};
inner.externalInputs = {struct('id', 'in1', 'name', 'in1')};
inner.externalOutputs = {struct('id', 'out1', 'name', 'out1')};

% Run the body once per element of the width-4 input: 3 * [1 2 3 4].
fe = struct('numIterations', 4);
fe.partition = {0}; % partition outer input port 0 (must be a cell -> JSON array)

d = struct();
d.blocks = {...
  struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, ...
    'params', struct('Value', [1 2 3 4])), ...
  struct('id', 'sub', 'type', 'subsystem', 'inputs', 1, 'outputs', 1, ...
    'params', struct('subsystem', inner, 'forEach', fe)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'y', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {...
  struct('from', 'c', 'to', 'sub', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'sub', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1;
d.duration = 0.2;

result = jsondecode(__nflow_simulate__(jsonencode(d)));
% result.signals.<blockId> holds each signal; the subsystem output is the
% width-4 concatenation of the per-element results.
sub = result.signals.sub;
lastRow = sub(end, :);
fprintf('for-each gain 3 over [1 2 3 4] -> [%g %g %g %g]\n', ...
  lastRow(1), lastRow(2), lastRow(3), lastRow(4));
%=============================================================================
