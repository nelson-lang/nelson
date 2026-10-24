%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Typed signals (logical + integers): saturation, wrap, boolean, convert.
%=============================================================================
% int8 saturation: constant 100 (int8) -> gain 2 -> 200 saturates to 127
d = struct();
d.blocks = {struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 100, 'OutDataType', 'int8')), ...
  struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, 'params', struct('Gain', 2)), ...
  struct('id', 'dp', 'type', 'display', 'inputs', 1, 'outputs', 0, 'params', struct())};
d.connections = {struct('from', 'c', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'g', 'to', 'dp', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.01; d.duration = 0.05; d.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
assert_isequal(nflow_test_sig(r, 'g'), 127);
assert_isequal(nflow_test_typ(r, 'g'), 'int8');
%=============================================================================
% wrap mode: same, gain declares SaturateOnOverflow=false -> 200 wraps to -56
d.blocks{2}.params.SaturateOnOverflow = false;
r2 = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r2.status, 'done');
assert_isequal(nflow_test_sig(r2, 'g'), -56);
%=============================================================================
% boolean logic: compareToZero over [1 0 3] -> not -> [0 1 0], type boolean
d3 = struct();
d3.blocks = {struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', [1 0 3])), ...
  struct('id', 'cmp', 'type', 'compareToZero', 'inputs', 1, 'outputs', 1, 'params', struct('Relop', 'ne')), ...
  struct('id', 'n', 'type', 'not', 'inputs', 1, 'outputs', 1, 'params', struct()), ...
  struct('id', 's', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct())};
d3.connections = {struct('from', 'c', 'to', 'cmp', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'cmp', 'to', 'n', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'n', 'to', 's', 'fromIndex', 0, 'toIndex', 0)};
d3.sampleTime = 0.01; d3.duration = 0.05; d3.variables = struct();
r3 = jsondecode(__nflow_simulate__(jsonencode(d3)));
assert_isequal(r3.status, 'done');
assert_isequal(nflow_test_sig(r3, 'n'), [0 1 0]);
assert_isequal(nflow_test_typ(r3, 'cmp'), 'boolean');
%=============================================================================
% type mismatch: int8 + double -> sum : clean error
d4 = struct();
d4.blocks = {struct('id', 'a', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 5, 'OutDataType', 'int8')), ...
  struct('id', 'b', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 2)), ...
  struct('id', 'sm', 'type', 'sum', 'inputs', 2, 'outputs', 1, 'params', struct()), ...
  struct('id', 'dp', 'type', 'display', 'inputs', 1, 'outputs', 0, 'params', struct())};
d4.connections = {struct('from', 'a', 'to', 'sm', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'b', 'to', 'sm', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 'sm', 'to', 'dp', 'fromIndex', 0, 'toIndex', 0)};
d4.sampleTime = 0.01; d4.duration = 0.05; d4.variables = struct();
r4 = jsondecode(__nflow_simulate__(jsonencode(d4)));
assert_isequal(r4.status, 'error');
assert_istrue(~isempty(strfind(r4.reason, 'type mismatch')));
%=============================================================================
% convert unblocks the mix: int8 -> convert(double) -> sum = 7
d5 = d4;
d5.blocks{end + 1} = struct('id', 'cv', 'type', 'convert', 'inputs', 1, 'outputs', 1, 'params', struct('OutDataType', 'double'));
d5.connections = {struct('from', 'a', 'to', 'cv', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'cv', 'to', 'sm', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'b', 'to', 'sm', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 'sm', 'to', 'dp', 'fromIndex', 0, 'toIndex', 0)};
r5 = jsondecode(__nflow_simulate__(jsonencode(d5)));
assert_isequal(r5.status, 'done');
assert_isequal(nflow_test_sig(r5, 'sm'), 7);
%=============================================================================
% convert to int16 with floor rounding: 3.9 -> 3
d6 = struct();
d6.blocks = {struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 3.9)), ...
  struct('id', 'cv', 'type', 'convert', 'inputs', 1, 'outputs', 1, 'params', struct('OutDataType', 'int16', 'Rounding', 'floor')), ...
  struct('id', 'dp', 'type', 'display', 'inputs', 1, 'outputs', 0, 'params', struct())};
d6.connections = {struct('from', 'c', 'to', 'cv', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'cv', 'to', 'dp', 'fromIndex', 0, 'toIndex', 0)};
d6.sampleTime = 0.01; d6.duration = 0.05; d6.variables = struct();
r6 = jsondecode(__nflow_simulate__(jsonencode(d6)));
assert_isequal(r6.status, 'done');
assert_isequal(nflow_test_sig(r6, 'cv'), 3);
%=============================================================================
% integer into a continuous block (integrator) requires double -> error
d7 = struct();
d7.blocks = {struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 1, 'OutDataType', 'int32')), ...
  struct('id', 'i', 'type', 'integrator', 'inputs', 1, 'outputs', 1, 'params', struct('Initial', 0)), ...
  struct('id', 's', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct())};
d7.connections = {struct('from', 'c', 'to', 'i', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'i', 'to', 's', 'fromIndex', 0, 'toIndex', 0)};
d7.sampleTime = 0.01; d7.duration = 0.05; d7.variables = struct();
r7 = jsondecode(__nflow_simulate__(jsonencode(d7)));
assert_isequal(r7.status, 'error');
assert_istrue(~isempty(strfind(r7.reason, 'requires real double')));
%=============================================================================
function v = nflow_test_sig(r, id)
  v = r.signals.(id);
  if iscell(v)
    v = cell2mat(v);
  end
  v = v(:)';
end
%=============================================================================
function t = nflow_test_typ(r, id)
  t = r.signalTypes.(id);
  if iscell(t)
    t = t{1};
  end
end
%=============================================================================
