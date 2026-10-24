%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A(...) = [] : with only ':' subscripts the first dimension is emptied
x = magic(3); x(:, :) = [];
asserts.isequal(size(x), [0 3]);
x = magic(3); x(1:3, :) = [];
asserts.isequal(size(x), [0 3]);
x = magic(3); x([1 2 3], :) = [];
asserts.isequal(size(x), [0 3]);
x = magic(3); x(:, :, :) = [];
asserts.isequal(size(x), [0 3]);
x = magic(3); x(:, :, 1) = [];
asserts.isequal(size(x), [3 3 0]);
x = magic(3); x(:, :, :, 1) = [];
asserts.isequal(size(x), [3 3 1 0]);
x = magic(3); x(:, 1:3) = [];
asserts.isequal(size(x), [3 0]);
%=============================================================================
% fewer subscripts than dimensions fold the trailing dimensions
x = zeros(2, 3, 4); x(:, :) = [];
asserts.isequal(size(x), [0 12]);
x = zeros(2, 3, 4); x(:, 1) = [];
asserts.isequal(size(x), [2 11]);
x = zeros(2, 3, 4); x(1, :) = [];
asserts.isequal(size(x), [1 12]);
x = zeros(2, 3, 4); x(:, :, :) = [];
asserts.isequal(size(x), [0 3 4]);
x = zeros(2, 3, 4); x(:, :, :, 1) = [];
asserts.isequal(size(x), [2 3 4 0]);
x = zeros(2, 3, 4); x(:, :, 1:4) = [];
asserts.isequal(size(x), [2 3 0]);
x = zeros(2, 3, 4); x(:, 1:3, :) = [];
asserts.isequal(size(x), [2 0 4]);
x = zeros(2, 3, 4); x(1:2, :, :) = [];
asserts.isequal(size(x), [0 3 4]);
x = zeros(2, 3, 4); x(:, :, [1 3]) = [];
asserts.isequal(size(x), [2 3 2]);
%=============================================================================
% only one subscript may differ from ':'
assert_checkerror('x = zeros(2, 3, 4); x(1, 1, :) = [];', _('Statement A(...) = [] can only contain one non-colon index.'));
assert_checkerror('x = zeros(2, 3, 4); x(1, :, 1) = [];', _('Statement A(...) = [] can only contain one non-colon index.'));
assert_checkerror('x = zeros(2, 3, 4); x(:, :, 5) = [];', _('Index exceeds array bounds.'));
%=============================================================================
% linear deletion: a scalar or a matrix becomes a row, a vector keeps its orientation
x = magic(3); x(1:end) = [];
asserts.isequal(size(x), [1 0]);
x = magic(3); x(true(3)) = [];
asserts.isequal(size(x), [1 0]);
x = zeros(2, 3, 4); x([1 2]) = [];
asserts.isequal(size(x), [1 22]);
x = 5; x(1) = [];
asserts.isequal(size(x), [1 0]);
x = 5; x(true) = [];
asserts.isequal(size(x), [1 0]);
x = 5; x(1, :) = [];
asserts.isequal(size(x), [0 1]);
x = 5; x(:, :) = [];
asserts.isequal(size(x), [0 1]);
x = 5; x(:, :, 1) = [];
asserts.isequal(size(x), [1 1 0]);
x = 5; x(1, :, :) = [];
asserts.isequal(size(x), [0 1]);
x = (1:5)'; x(1:5) = [];
asserts.isequal(size(x), [0 1]);
x = 1:5; x(1:5) = [];
asserts.isequal(size(x), [1 0]);
x = 1:5; x(:) = [];
asserts.isequal(size(x), [0 0]);
x = (1:5)'; x(:) = [];
asserts.isequal(size(x), [0 0]);
%=============================================================================
% the class is kept
c = {1, 2, 3}; c(:) = [];
asserts.isequal(class(c), 'cell');
asserts.isequal(size(c), [0 0]);
s = struct('a', {1, 2}); s(:) = [];
asserts.isequal(class(s), 'struct');
asserts.isequal(size(s), [0 0]);
x = ["a" "b"]; x(:) = [];
asserts.isequal(class(x), 'string');
x = 'abc'; x(:) = [];
asserts.isequal(class(x), 'char');
asserts.isequal(size(x), [0 0]);
x = int8([1 2]); x(:) = [];
asserts.isequal(class(x), 'int8');
x = true(1, 2); x(:) = [];
asserts.isequal(class(x), 'logical');
x = single([1 2]); x(:) = [];
asserts.isequal(class(x), 'single');
%=============================================================================
% the remaining elements may hold no imaginary part
x = [1+2i 3]; x(1) = [];
asserts.istrue(isreal(x));
asserts.isequal(x, 3);
%=============================================================================
% nothing can be deleted from an empty array by index
assert_checkerror('x = []; x(1) = [];', _('Index exceeds array bounds.'));
assert_checkerror('x = zeros(1, 0); x(1) = [];', _('Index exceeds array bounds.'));
assert_checkerror('x = zeros(0, 3); x(1, :) = [];', _('Index exceeds array bounds.'));
x = zeros(1, 0); x(1, :) = [];
asserts.isequal(size(x), [0 0]);
x = zeros(2, 0); x(1, :) = [];
asserts.isequal(size(x), [1 0]);
x = zeros(3, 0); x(1:3, :) = [];
asserts.isequal(size(x), [0 0]);
x = zeros(3, 0); x(:, :) = [];
asserts.isequal(size(x), [0 0]);
x = zeros(0, 3); x(:, 1:3) = [];
asserts.isequal(size(x), [0 0]);
x = zeros(1, 0); x(:, :) = [];
asserts.isequal(size(x), [0 0]);
x = []; x(:, :) = [];
asserts.isequal(size(x), [0 0]);
x = zeros(0, 3); x(:, 1) = [];
asserts.isequal(size(x), [0 2]);
%=============================================================================
% a subscript covering its whole dimension is not a ':'
assert_checkerror('x = magic(3); x(1:3, 1:3) = [];', _('Statement A(...) = [] can only contain one non-colon index.'));
assert_checkerror('x = magic(3); x(1:3, 1) = [];', _('Statement A(...) = [] can only contain one non-colon index.'));
assert_checkerror('x = magic(3); x(1, 1:3) = [];', _('Statement A(...) = [] can only contain one non-colon index.'));
assert_checkerror('x = magic(3); x([1 2 3], [1 2 3]) = [];', _('Statement A(...) = [] can only contain one non-colon index.'));
assert_checkerror('x = 5; x(1, 1) = [];', _('Statement A(...) = [] can only contain one non-colon index.'));
assert_checkerror('x = 1:5; x(1, 2) = [];', _('Statement A(...) = [] can only contain one non-colon index.'));
assert_checkerror('x = (1:5)''; x(1, 1) = [];', _('Statement A(...) = [] can only contain one non-colon index.'));
assert_checkerror('x = (1:5)''; x(2, 1) = [];', _('Statement A(...) = [] can only contain one non-colon index.'));
assert_checkerror('x = zeros(2, 3, 4); x(1:2, 1) = [];', _('Statement A(...) = [] can only contain one non-colon index.'));
x = zeros(2, 3, 4); x(:, 1:12) = [];
asserts.isequal(size(x), [2 0]);
x = zeros(2, 3, 4); x(1:2, :) = [];
asserts.isequal(size(x), [0 12]);
%=============================================================================
% an empty subscript deletes nothing
x = magic(3); x(:, []) = [];
asserts.isequal(x, magic(3));
x = magic(3); x([], :) = [];
asserts.isequal(x, magic(3));
x = zeros(2, 3, 4); x(:, []) = [];
asserts.isequal(size(x), [2 3 4]);
x = zeros(2, 3, 4); x(zeros(1, 0), :) = [];
asserts.isequal(size(x), [2 3 4]);
x = magic(3); x([]) = [];
asserts.isequal(size(x), [1 9]);
x = zeros(2, 3, 4); x([]) = [];
asserts.isequal(size(x), [1 24]);
x = 1:5; x([]) = [];
asserts.isequal(x, 1:5);
%=============================================================================
% on an empty array an empty subscript is accepted whatever the other subscripts
x = zeros(1, 0); x(1, []) = [];
asserts.isequal(size(x), [1 0]);
x = zeros(0, 3); x([], 1) = [];
asserts.isequal(size(x), [0 3]);
x = []; x(1, []) = [];
asserts.isequal(size(x), [0 0]);
x = []; x(1, 1:end) = [];
asserts.isequal(size(x), [0 0]);
%=============================================================================
