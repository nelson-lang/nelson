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
% busAssignment: replaces selected bus members and passes the rest through.
% Bus {pos (width 2), count (scalar)} built by busCreator; busAssignment
% overwrites a subset; busSelector reads the members back out. Row =
% {assigned-paths, replacement-values, expected-pos, expected-count}.
%=============================================================================
cases = {...
  {{'count'}, {99}, [1 2], 7, [1 2], 99}, ...
  {{'pos'}, {[30 40]}, [1 2], 7, [30 40], 7}, ...
  {{'pos', 'count'}, {[5 6], 8}, [1 2], 7, [5 6], 8}};
for t = 1:numel(cases)
  c = cases{t}; assigned = c{1}; repl = c{2}; expP = c{5}; expC = c{6};
  blocks = {...
    struct('id', 'pos', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', [1 2])), ...
    struct('id', 'cnt', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 7)), ...
    struct('id', 'bc', 'type', 'busCreator', 'inputs', 2, 'outputs', 1, 'params', struct('MemberNames', {{'pos', 'count'}})), ...
    struct('id', 'ba', 'type', 'busAssignment', 'inputs', 1 + numel(assigned), 'outputs', 1, 'params', struct('AssignedSignals', {assigned})), ...
    struct('id', 'sel', 'type', 'busSelector', 'inputs', 1, 'outputs', 2, 'params', struct('SelectedSignals', {{'pos', 'count'}})), ...
    struct('id', 'wp', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, 'params', struct('VariableName', 'vp', 'SaveFormat', 'Array')), ...
    struct('id', 'wc', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, 'params', struct('VariableName', 'vc', 'SaveFormat', 'Array'))};
  conns = {...
    struct('from', 'pos', 'to', 'bc', 'fromIndex', 0, 'toIndex', 0), struct('from', 'cnt', 'to', 'bc', 'fromIndex', 0, 'toIndex', 1), ...
    struct('from', 'bc', 'to', 'ba', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'ba', 'to', 'sel', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'sel', 'to', 'wp', 'fromIndex', 0, 'toIndex', 0), struct('from', 'sel', 'to', 'wc', 'fromIndex', 1, 'toIndex', 0)};
  for j = 1:numel(assigned)
    id = ['r', int2str(j)];
    blocks{end + 1} = struct('id', id, 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', repl{j}));
    conns{end + 1} = struct('from', id, 'to', 'ba', 'fromIndex', 0, 'toIndex', j);
  end
  d = struct(); d.blocks = blocks; d.connections = conns;
  d.sampleTime = 0.1; d.duration = 0.1; d.solver = 'discrete'; d.variables = struct();
  res = jsondecode(__nflow_simulate__(jsonencode(d)));
  assert_isequal(res.status, 'done');
  vp = evalin('base', 'vp'); vp = vp(1, :);
  vc = evalin('base', 'vc'); vc = vc(1, :);
  assert_isequal(vp, expP);
  assert_isequal(vc, expC);
end
%=============================================================================
