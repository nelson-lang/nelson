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
% A block with its own SampleTime inside a subsystem was refused outright:
% multi-rate was described as a top-level feature. That is true of the blocks
% the ENGINE schedules - a unit delay, a counter, an edge detector - which need
% a rate table the inner diagram never sees. It is not true of the blocks that
% carry their own clock and gate their own advance on it (zoh, foh, ddelay,
% dtf, dstateSpace): those work one level down exactly as they do at the top,
% and are now allowed there.
%=============================================================================
function y = ss_rate_run(innerBlock, stop, dt)
  inner = struct();
  inner.blocks = {...
    struct('id', 'in1', 'type', 'labelSource', 'inputs', 0, 'outputs', 1, ...
      'params', struct('label', 'in1')), ...
    innerBlock, ...
    struct('id', 'out1', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
      'params', struct('label', 'out1'))};
  inner.connections = {...
    struct('from', 'in1', 'to', innerBlock.id, 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', innerBlock.id, 'to', 'out1', 'fromIndex', 0, 'toIndex', 0)};
  inner.externalInputs = {struct('id', 'in1', 'name', 'in1')};
  inner.externalOutputs = {struct('id', 'out1', 'name', 'out1')};
  d = struct();
  d.version = '1.0.0';
  d.format = 'nflow-2';
  d.name = 'subsystem_self_scheduled';
  d.runtime = stop;
  d.sampleTime = dt;
  d.blocks = {...
    struct('id', 'r', 'type', 'ramp', 'x', 40, 'y', 100, 'rotation', 0, ...
      'params', struct('slope', 1)), ...
    struct('id', 's', 'type', 'subsystem', 'x', 240, 'y', 100, 'rotation', 0, ...
      'params', struct('subsystem', inner)), ...
    struct('id', 'tw', 'type', 'toWorkspace', 'x', 440, 'y', 100, 'rotation', 0, ...
      'params', struct('VariableName', 'y_sr', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
  d.connections = {...
    struct('from', 'r', 'to', 's', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 's', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
  model = [tempname(), '.nflow'];
  fid = fopen(model, 'wt');
  fwrite(fid, jsonencode(d));
  fclose(fid);
  out = sim(model);
  rmfile(model);
  y = out.y_sr(:)';
  evalin('base', 'clear(''y_sr'')');
end
%=============================================================================
% A zero-order hold at 0.2 s inside a subsystem, on a base step of 0.05: it
% samples the ramp every fourth base step and holds in between.
z = struct('id', 'z', 'type', 'zoh', 'inputs', 1, 'outputs', 1, ...
  'params', struct('SampleTime', 0.2));
y = ss_rate_run(z, 0.8, 0.05);
assert_isequal(numel(y), 17);
% four base steps to a sample: 0, 0, 0, 0, 0.2, 0.2, 0.2, 0.2, 0.4, ...
expected = [0, 0, 0, 0, 0.2, 0.2, 0.2, 0.2, 0.4, 0.4, 0.4, 0.4, 0.6, 0.6, ...
  0.6, 0.6, 0.8];
assert_istrue(max(abs(y - expected)) < 1e-9);
%=============================================================================
% The same block at the base rate follows the ramp sample by sample, so the
% staircase above is the rate doing its work and not a coincidence.
z2 = struct('id', 'z', 'type', 'zoh', 'inputs', 1, 'outputs', 1, ...
  'params', struct('SampleTime', 0.05));
y2 = ss_rate_run(z2, 0.5, 0.05);
assert_istrue(max(abs(y2 - (0:0.05:0.5))) < 1e-9);
%=============================================================================
% A block the ENGINE schedules keeps the refusal, and says why: its rate table
% is built for the top level and the inner diagram never sees it.
u = struct('id', 'u', 'type', 'unitDelay', 'inputs', 1, 'outputs', 1, ...
  'params', struct('InitialCondition', 0, 'SampleTime', 0.2));
msg = '';
try
  ss_rate_run(u, 0.5, 0.05);
catch e
  msg = e.message;
end
assert_istrue(~isempty(strfind(msg, 'nested inside a subsystem')));
%=============================================================================
