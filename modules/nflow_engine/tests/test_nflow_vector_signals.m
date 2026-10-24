%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Vector/matrix signals through the simulation engine.
%=============================================================================
% constant [1 2 3] -> gain 2 -> scope : vector propagates, gain is element-wise
d = struct();
d.blocks = {struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', [1 2 3])), ...
  struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, 'params', struct('Gain', 2)), ...
  struct('id', 's', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct())};
d.connections = {struct('from', 'c', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'g', 'to', 's', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.01; d.duration = 0.05; d.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
assert_isequal(nflow_test_sig(r, 'g'), [2 4 6]);
assert_isequal(r.signalDims.g(:)', [3 1]);
%=============================================================================
% real mux/demux: 2 scalars -> mux -> demux -> displays
d2 = struct();
d2.blocks = {struct('id', 'a', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 10)), ...
  struct('id', 'b', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 20)), ...
  struct('id', 'm', 'type', 'mux', 'inputs', 2, 'outputs', 1, 'params', struct()), ...
  struct('id', 'dm', 'type', 'demux', 'inputs', 1, 'outputs', 2, 'params', struct()), ...
  struct('id', 'd1', 'type', 'display', 'inputs', 1, 'outputs', 0, 'params', struct()), ...
  struct('id', 'd2', 'type', 'display', 'inputs', 1, 'outputs', 0, 'params', struct())};
d2.connections = {struct('from', 'a', 'to', 'm', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'b', 'to', 'm', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 'm', 'to', 'dm', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'dm', 'to', 'd1', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'dm', 'to', 'd2', 'fromIndex', 1, 'toIndex', 0)};
d2.sampleTime = 0.01; d2.duration = 0.05; d2.variables = struct();
r2 = jsondecode(__nflow_simulate__(jsonencode(d2)));
assert_isequal(r2.status, 'done');
assert_isequal(nflow_test_sig(r2, 'm'), [10 20]);
%=============================================================================
% selector [4 1] over [5 6 7 8] -> [8 5]
d3 = struct();
d3.blocks = {struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', [5 6 7 8])), ...
  struct('id', 'sel', 'type', 'selector', 'inputs', 1, 'outputs', 1, 'params', struct('Indices', [4 1])), ...
  struct('id', 's', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct())};
d3.connections = {struct('from', 'c', 'to', 'sel', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'sel', 'to', 's', 'fromIndex', 0, 'toIndex', 0)};
d3.sampleTime = 0.01; d3.duration = 0.05; d3.variables = struct();
r3 = jsondecode(__nflow_simulate__(jsonencode(d3)));
assert_isequal(r3.status, 'done');
assert_isequal(nflow_test_sig(r3, 'sel'), [8 5]);
%=============================================================================
% vector integrator: constant [1 2] -> integrator -> approaches slope
d4 = struct();
d4.blocks = {struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', [1 2])), ...
  struct('id', 'i', 'type', 'integrator', 'inputs', 1, 'outputs', 1, 'params', struct('Initial', 0)), ...
  struct('id', 's', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct())};
d4.connections = {struct('from', 'c', 'to', 'i', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'i', 'to', 's', 'fromIndex', 0, 'toIndex', 0)};
d4.sampleTime = 0.005; d4.duration = 1.0; d4.variables = struct();
r4 = jsondecode(__nflow_simulate__(jsonencode(d4)));
assert_isequal(r4.status, 'done');
iv = nflow_test_sig(r4, 'i');
assert_istrue(abs(iv(1) - 1) < 0.02 && abs(iv(2) - 2) < 0.02);
%=============================================================================
% dimension mismatch: [1 2 3] + [1 2] -> sum : clean error naming the block
d5 = struct();
d5.blocks = {struct('id', 'x', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', [1 2 3])), ...
  struct('id', 'y', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', [1 2])), ...
  struct('id', 'sum1', 'type', 'sum', 'inputs', 2, 'outputs', 1, 'params', struct()), ...
  struct('id', 's', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct())};
d5.connections = {struct('from', 'x', 'to', 'sum1', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'y', 'to', 'sum1', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 'sum1', 'to', 's', 'fromIndex', 0, 'toIndex', 0)};
d5.sampleTime = 0.01; d5.duration = 0.05; d5.variables = struct();
r5 = jsondecode(__nflow_simulate__(jsonencode(d5)));
assert_isequal(r5.status, 'error');
assert_istrue(~isempty(strfind(r5.reason, 'width mismatch')));
%=============================================================================
function v = nflow_test_sig(r, id)
  v = r.signals.(id);
  if iscell(v)
    v = cell2mat(v);
  end
  v = v(:)';
end
%=============================================================================
