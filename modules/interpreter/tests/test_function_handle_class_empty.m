%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% `@TypeName.empty` is a handle to the static empty-array constructor.
%=============================================================================
g = @double.empty;
x = g(0, 2);
assert_isequal(class(x), 'double');
assert_isequal(size(x), [0 2]);
assert_isequal(size(g()), [0 0]);
assert_isequal(size(g([3 0 2])), [3 0 2]);
assert_isequal(func2str(g), 'double.empty');
R = evalc('disp(g)');
assert_istrue(contains(R, '@double.empty'));
%=============================================================================
h = @function_handle.empty;
y = h();
assert_isequal(class(y), 'function_handle');
assert_isequal(size(y), [0 0]);
%=============================================================================
h = @string.empty;
y = h(1, 0);
assert_isequal(class(y), 'string');
assert_isequal(size(y), [1 0]);
%=============================================================================
h = @struct.empty;
y = h();
assert_isequal(class(y), 'struct');
assert_isequal(size(y), [0 0]);
%=============================================================================
y = feval(@int8.empty, 3, 0);
assert_isequal(class(y), 'int8');
assert_isequal(size(y), [3 0]);
%=============================================================================
g2 = str2func('logical.empty');
y = g2(2, 0);
assert_isequal(class(y), 'logical');
assert_isequal(size(y), [2 0]);
assert_isequal(func2str(g2), 'logical.empty');
%=============================================================================
C = cellfun(@cell.empty, {0, 2}, {3, 0}, 'UniformOutput', false);
assert_isequal(class(C{1}), 'cell');
assert_isequal(size(C{1}), [0 3]);
assert_isequal(size(C{2}), [2 0]);
%=============================================================================
h = @datetime.empty;
y = h(0, 3);
assert_isequal(class(y), 'datetime');
assert_isequal(size(y), [0 3]);
assert_isequal(func2str(h), 'datetime.empty');
%=============================================================================
h = @categorical.empty;
y = h();
assert_isequal(class(y), 'categorical');
assert_isequal(size(y), [0 0]);
%=============================================================================
g = @double.empty;
assert_checkerror('g(2, 2)', _('At least one dimension must be zero to create an empty array.'), 'Nelson:emptyArray:notEmpty');
%=============================================================================
