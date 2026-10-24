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
% A for-iterator subsystem put its body's state back to its initial condition
% at every step entry, and there was no way to ask for anything else: a counter
% inside one could only ever count within a step. ResetStates 'held' carries
% that state across steps like any other discrete state; 'reset' is the
% default and is what the block has always done.
%=============================================================================
function y = fi_run(n, policy)
  inner = struct();
  inner.blocks = {...
    struct('id', 'in1', 'type', 'labelSource', 'inputs', 0, 'outputs', 1, ...
      'params', struct('label', 'in1')), ...
    struct('id', 'd', 'type', 'unitDelay', 'inputs', 1, 'outputs', 1, ...
      'params', struct('InitialCondition', 0)), ...
    struct('id', 'a', 'type', 'sum', 'inputs', 2, 'outputs', 1, ...
      'params', struct('Inputs', '++')), ...
    struct('id', 'out1', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
      'params', struct('label', 'out1'))};
  inner.connections = {...
    struct('from', 'in1', 'to', 'a', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'd', 'to', 'a', 'fromIndex', 0, 'toIndex', 1), ...
    struct('from', 'a', 'to', 'd', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'a', 'to', 'out1', 'fromIndex', 0, 'toIndex', 0)};
  inner.externalInputs = {struct('id', 'in1', 'name', 'in1')};
  inner.externalOutputs = {struct('id', 'out1', 'name', 'out1')};

  cp = struct('kind', 'forIterator', 'IterationSource', 'internal', ...
    'IterationLimit', n);
  if ~isempty(policy)
    cp.ResetStates = policy;
  end
  d = struct();
  d.version = '1.0.0';
  d.format = 'nflow-2';
  d.name = 'for_iterator_states';
  d.runtime = 1.0;
  d.sampleTime = 0.1;
  d.blocks = {...
    struct('id', 'u', 'type', 'constant', 'x', 40, 'y', 100, 'rotation', 0, ...
      'params', struct('Value', 1)), ...
    struct('id', 's', 'type', 'subsystem', 'x', 240, 'y', 100, 'rotation', 0, ...
      'params', struct('subsystem', inner, 'controlPort', cp)), ...
    struct('id', 'tw', 'type', 'toWorkspace', 'x', 440, 'y', 100, 'rotation', 0, ...
      'params', struct('VariableName', 'y_fi', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
  d.connections = {...
    struct('from', 'u', 'to', 's', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 's', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
  model = [tempname(), '.nflow'];
  fid = fopen(model, 'wt');
  fwrite(fid, jsonencode(d));
  fclose(fid);
  out = sim(model);
  rmfile(model);
  y = out.y_fi(:)';
  evalin('base', 'clear(''y_fi'')');
end
%=============================================================================
% Held: the body counts on from where the previous step left it, so after k
% samples of n iterations each the counter reads n k.
for n = [2, 4]
  y = fi_run(n, 'held');
  assert_isequal(numel(y), 11);
  assert_istrue(max(abs(y - n * (1:11))) < 1e-9);
end
%=============================================================================
% Reset: the body starts over at every step entry, so it never gets past n.
for n = [2, 4]
  y = fi_run(n, 'reset');
  assert_isequal(numel(y), 11);
  assert_istrue(max(abs(y - n)) < 1e-9);
end
%=============================================================================
% Reset is the default, so a diagram that says nothing behaves as it always
% did.
for n = [2, 4]
  assert_istrue(max(abs(fi_run(n, '') - n)) < 1e-9);
end
%=============================================================================
