%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
savedPath = path();
pathCleanup = onCleanup(@() path(savedPath));
addpath(modulepath('interpreter', 'tests'));
asserts.isequal(stack_order(11, 22, 33), [11, 22, 33]);
values = {11, 22, 33};
asserts.isequal(stack_order(values{:}), [11, 22, 33]);
[a, b, c] = stack_outputs(values{:});
asserts.isequal([a, b, c], [11, 22, 33]);
asserts.isequal([1, 2; 3, 4], reshape([1, 3, 2, 4], 2, 2));
asserts.isequal({11, 22; 33, 44}, reshape({11, 33, 22, 44}, 2, 2));
asserts.isequal(stack_order(single(1), single(2), single(3)), single([1, 2, 3]));
asserts.isequal(full(stack_order(sparse(1), sparse(2), sparse(3))), [1, 2, 3]);
asserts.isequal(stack_order(1i, 2i, 3i), [1i, 2i, 3i]);
asserts.isequal(stack_order([], [1, 2], []), [1, 2]);
asserts.isequal([1, stack_order([], [], []), 2], [1, 2]);
asserts.isequal([11, empty_stack_value(), 22], [11, 22]);
asserts.isequal([], zeros(0, 0));
asserts.isequal([zeros(0, 3)], zeros(0, 3));
asserts.isequal([true, false], logical([1, 0]));
asserts.isequal([uint8(1), uint8(2)], uint8([1, 2]));
asserts.istrue(issparse([sparse([1, 2])]));

% A transferred stack value must not alter the source or its other copies.
for repetition = 1:10
  m = [1, 1];
  alias = m;
  m = m;
  n = stack_order(m, m, m);
  n(1) = 7;
  asserts.isequal(m, [1, 1]);
  asserts.isequal(alias, [1, 1]);
  asserts.isequal(n, [7, 1, 1, 1, 1, 1]);
end

inputs = {[1, 2], single([1, 2]), true, uint8([1, 2]), 1i, sparse([1, 2]), ...
  [], {1, 2}, struct('Value', 7)};
for inputIndex = 1:numel(inputs)
  value = inputs{inputIndex};
  before = value;
  for repetition = 1:20
    value = value;
  end
  asserts.isequal(value, before);
end

asserts.checkerror('stack_locked_identity();', _('Redefining permanent variable.'));

profile('clear');

savedBreakpoints = dbstatus();
breakpointCleanup = onCleanup(@() restore_stack_breakpoints(savedBreakpoints));
dbstop in bytecode_local_scalar_plus at 13
value = [1, 1];
for repetition = 1:20
  value = value;
end
asserts.isequal(value, [1, 1]);
clear breakpointCleanup;
value = [1, 1];
profile('on');
for repetition = 1:5
  value = value;
end
profile('off');
p = profile('info');
assignmentEntries = strcmp({p.FunctionName}, 'subsasgn');
asserts.isequal(sum([p(assignmentEntries).NumCalls]), 5);
profile('clear');

if ~iswasm()
  directory = tempname();
  mkdir(directory);
  directoryCleanup = onCleanup(@() rmdir(directory, 's'));
  source = [directory, '/stack_identity.m'];
  artifactDirectory = [directory, '/compiled'];
  mkdir(artifactDirectory);
  artifact = [artifactDirectory, '/stack_identity.nbc'];
  filewrite(source, {'function out = stack_identity(n)', ...
    'n = n;', 'out = n;', 'end'});
  __bytecode_artifact__('compile', source, artifact);
  inspection = __bytecode_artifact__('inspect', artifact);
  asserts.istrue(contains(inspection, 'SCALAR_EXPR'));
  addpath(artifactDirectory);
  for inputIndex = 1:numel(inputs)
    asserts.isequal(stack_identity(inputs{inputIndex}), inputs{inputIndex});
  end
  rmpath(artifactDirectory);
end

function out = stack_order(a, b, c)
  out = [a, b, c];
end

function [a, b, c] = stack_outputs(a, b, c)
end

function out = empty_stack_value()
  out = [];
end

function stack_locked_identity()
  value = [1, 2];
  varlock('local', 'value');
  value = value;
end

function restore_stack_breakpoints(saved)
  dbclear('all');
  if ~isempty(saved)
    dbstop(saved);
  end
end
