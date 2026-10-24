%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% TypeName.empty used directly as a call argument keeps its lowercase class
% name, at script level, inside an anonymous function and through disp.
%=============================================================================
names = {'string', 'struct', 'missing', 'cell', 'double', 'single', ...
    'logical', 'char', 'int8', 'uint64', 'function_handle'};
for k = 1:numel(names)
  R = eval(['class(', names{k}, '.empty)']);
  assert_isequal(R, names{k});
end
%=============================================================================
assert_isequal(class(string.empty), 'string');
assert_isequal(class(struct.empty), 'struct');
assert_isequal(class(missing.empty), 'missing');
assert_isequal(class(cell.empty), 'cell');
assert_isequal(class(double.empty), 'double');
%=============================================================================
f = @() class(string.empty);
assert_isequal(f(), 'string');
f = @() class(missing.empty(0, 3));
assert_isequal(f(), 'missing');
%=============================================================================
R = evalc('disp(class(string.empty))');
assert_isequal(R, sprintf('string\n'));
R = evalc('disp(class(struct.empty))');
assert_isequal(R, sprintf('struct\n'));
%=============================================================================
