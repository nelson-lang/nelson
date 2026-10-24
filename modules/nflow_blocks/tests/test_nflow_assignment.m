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
% assignment: out = base (port 0), with out[Indices[k]] = values[k] (port 1).
% Row = {base, values, Indices, expected}. Compact form on purpose.
%=============================================================================
cases = {...
  {[1 2 3 4], [90 70], [2 4], [1 90 3 70]}, ...
  {[1 2 3], [8], [1], [8 2 3]}, ...
  {[5 5 5 5], [1 2 3 4], [1 2 3 4], [1 2 3 4]}, ...
  {[0 0 0], [9 9], [3 1], [9 0 9]}};
for k = 1:numel(cases)
  c = cases{k}; base = c{1}; vals = c{2}; idx = c{3}; expected = c{4};
  vn = ['as', int2str(k)];
  d = struct();
  d.blocks = {struct('id', 'b', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', base)), struct('id', 'v', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', vals)), struct('id', 'a', 'type', 'assignment', 'inputs', 2, 'outputs', 1, 'params', struct('Indices', idx)), struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, 'params', struct('VariableName', vn, 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
  d.connections = {struct('from', 'b', 'to', 'a', 'fromIndex', 0, 'toIndex', 0), struct('from', 'v', 'to', 'a', 'fromIndex', 0, 'toIndex', 1), struct('from', 'a', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
  d.sampleTime = 0.1; d.duration = 0.1; d.solver = 'discrete'; d.variables = struct();
  res = jsondecode(__nflow_simulate__(jsonencode(d)));
  assert_isequal(res.status, 'done');
  y = evalin('base', vn); y = y(1, :);
  assert_isequal(y, expected);
end
%=============================================================================
