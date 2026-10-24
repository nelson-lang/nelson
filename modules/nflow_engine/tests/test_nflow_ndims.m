%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% N-D (> 2 dims) signals through the simulation engine.
%=============================================================================
% constant 1:12 -> reshape [2 3 2] -> gain 2 : N-D shape propagates, values
% are element-wise doubled, signalDims carries the full shape.
d = struct();
d.blocks = {struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 1:12)), ...
  struct('id', 'rs', 'type', 'reshape', 'inputs', 1, 'outputs', 1, 'params', struct('OutputDimensions', [2 3 2])), ...
  struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, 'params', struct('Gain', 2)), ...
  struct('id', 's', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct())};
d.connections = {struct('from', 'c', 'to', 'rs', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'rs', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'g', 'to', 's', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.01; d.duration = 0.05; d.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
assert_isequal(r.signalDims.rs(:)', [2 3 2]);
assert_isequal(r.signalDims.g(:)', [2 3 2]);
assert_isequal(nflow_test_sig(r, 'g'), 2 * (1:12));
%=============================================================================
% concatenate along dimension 3: two [2x3] pages -> [2x3x2], column-major
% order preserved (page 1 = first input, page 2 = second input).
d2 = struct();
d2.blocks = {struct('id', 'a', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 1:6)), ...
  struct('id', 'b', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 7:12)), ...
  struct('id', 'ra', 'type', 'reshape', 'inputs', 1, 'outputs', 1, 'params', struct('OutputDimensions', [2 3])), ...
  struct('id', 'rb', 'type', 'reshape', 'inputs', 1, 'outputs', 1, 'params', struct('OutputDimensions', [2 3])), ...
  struct('id', 'cat', 'type', 'concatenate', 'inputs', 2, 'outputs', 1, 'params', struct('ConcatenateDimension', 3)), ...
  struct('id', 's', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct())};
d2.connections = {struct('from', 'a', 'to', 'ra', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'b', 'to', 'rb', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ra', 'to', 'cat', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'rb', 'to', 'cat', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 'cat', 'to', 's', 'fromIndex', 0, 'toIndex', 0)};
d2.sampleTime = 0.01; d2.duration = 0.05; d2.variables = struct();
r2 = jsondecode(__nflow_simulate__(jsonencode(d2)));
assert_isequal(r2.status, 'done');
assert_isequal(r2.signalDims.cat(:)', [2 3 2]);
assert_isequal(nflow_test_sig(r2, 'cat'), 1:12);
%=============================================================================
% reshape that does not preserve the element count is a clean compile error.
d3 = struct();
d3.blocks = {struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 1:12)), ...
  struct('id', 'rs', 'type', 'reshape', 'inputs', 1, 'outputs', 1, 'params', struct('OutputDimensions', [2 3 3])), ...
  struct('id', 's', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct())};
d3.connections = {struct('from', 'c', 'to', 'rs', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'rs', 'to', 's', 'fromIndex', 0, 'toIndex', 0)};
d3.sampleTime = 0.01; d3.duration = 0.05; d3.variables = struct();
r3 = jsondecode(__nflow_simulate__(jsonencode(d3)));
assert_isequal(r3.status, 'error');
assert_istrue(~isempty(strfind(r3.reason, 'element count')));
%=============================================================================
function v = nflow_test_sig(r, id)
  v = r.signals.(id);
  if iscell(v)
    v = cell2mat(v);
  end
  v = v(:)';
end
%=============================================================================
