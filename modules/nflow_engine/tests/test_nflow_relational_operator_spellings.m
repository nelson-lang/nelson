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
% The comparison blocks take the operator either by name (ge/gt/le/lt/eq/ne, what
% the editor writes) or as a symbol. '~=' is how Nelson itself spells inequality,
% so a model written by script naturally carries it - and it used to fall through
% to the '>=' default and silently compute a different predicate: with equal
% operands it answered 1 (1 >= 1) where inequality is false.
%=============================================================================
cases = {...
  {'ne', 1, 1, 0}, {'!=', 1, 1, 0}, {'~=', 1, 1, 0}, ...
  {'ne', 1, 2, 1}, {'!=', 1, 2, 1}, {'~=', 1, 2, 1}, ...
  {'ge', 1, 2, 0}, {'lt', 1, 2, 1}};

for k = 1:numel(cases)
  op = cases{k}{1};
  a = cases{k}{2};
  b = cases{k}{3};
  expected = cases{k}{4};

  doc = struct();
  doc.version = '1.0.0';
  doc.format = 'nflow-2';
  doc.name = 'relop_spelling';
  doc.runtime = 0.05;
  doc.sampleTime = 0.01;
  doc.blocks = {...
    struct('id', 'a', 'type', 'constant', 'x', 40, 'y', 100, 'rotation', 0, ...
      'params', struct('Value', a)), ...
    struct('id', 'b', 'type', 'constant', 'x', 40, 'y', 220, 'rotation', 0, ...
      'params', struct('Value', b)), ...
    struct('id', 'r', 'type', 'relationalOperator', 'x', 240, 'y', 160, ...
      'rotation', 0, 'params', struct('Operator', op)), ...
    struct('id', 'tw', 'type', 'toWorkspace', 'x', 440, 'y', 160, 'rotation', 0, ...
      'params', struct('VariableName', 'y_relop', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
  doc.connections = {...
    struct('from', 'a', 'to', 'r', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'b', 'to', 'r', 'fromIndex', 0, 'toIndex', 1), ...
    struct('from', 'r', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};

  model = [tempname(), '.nflow'];
  fid = fopen(model, 'wt');
  fwrite(fid, jsonencode(doc));
  fclose(fid);
  out = sim(model);
  asserts.isequal(out.y_relop(end), expected);
  rmfile(model);
end
%=============================================================================
