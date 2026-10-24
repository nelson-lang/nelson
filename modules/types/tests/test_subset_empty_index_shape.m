%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A([]) follows the shape of the index; an empty vector index keeps the
% orientation of a vector
x = 1:5;
asserts.isequal(size(x([])), [0 0]);
asserts.isequal(size(x(zeros(0, 1))), [1 0]);
asserts.isequal(size(x(zeros(1, 0))), [1 0]);
x = (1:5)';
asserts.isequal(size(x([])), [0 0]);
asserts.isequal(size(x(zeros(1, 0))), [0 1]);
x = magic(3);
asserts.isequal(size(x([])), [0 0]);
asserts.isequal(size(x(zeros(0, 1))), [0 1]);
x = 'abc';
R = x([]);
asserts.isequal(size(R), [0 0]);
asserts.isequal(class(R), 'char');
x = ["a" "b" "c"];
R = x([]);
asserts.isequal(size(R), [0 0]);
asserts.isequal(class(R), 'string');
s = struct('a', {1, 2, 3});
R = s([]);
asserts.isequal(size(R), [0 0]);
asserts.isequal(class(R), 'struct');
%=============================================================================
% fewer subscripts than dimensions on an empty N-D array
x = zeros(0, 3, 2);
asserts.isequal(size(x(:, :)), [0 6]);
asserts.isequal(size(x(:, 6)), [0 1]);
assert_checkerror('x = zeros(0, 3, 2); y = x(:, 7);', _('Index exceeds array bounds.'));
%=============================================================================
% an empty vector is still a vector: it keeps its orientation
x = zeros(1, 0);
asserts.isequal(size(x(zeros(0, 1))), [1 0]);
asserts.isequal(size(x(zeros(1, 0))), [1 0]);
asserts.isequal(size(x([])), [0 0]);
x = zeros(0, 1);
asserts.isequal(size(x(zeros(1, 0))), [0 1]);
asserts.isequal(size(x(1:0)), [0 1]);
x = cell(0, 1);
asserts.isequal(size(x(zeros(1, 0))), [0 1]);
x = cell(1, 0);
asserts.isequal(size(x(zeros(0, 1))), [1 0]);
x = char(zeros(1, 0));
asserts.isequal(size(x(zeros(0, 1))), [1 0]);
x = zeros(0, 3);
asserts.isequal(size(x(zeros(1, 0))), [1 0]);
%=============================================================================
%=============================================================================
% the other subscripts of an empty selection must still be in range
x = ones(3);
assert_checkerror('y = x([], 4);', _('Index exceeds array bounds.'));
assert_checkerror('y = x(4, []);', _('Index exceeds array bounds.'));
x = ones(1, 4);
assert_checkerror('y = x([1 2], []);', _('Index exceeds array bounds.'));
assert_checkerror('y = x([], 5);', _('Index exceeds array bounds.'));
asserts.isequal(size(x([], [1 2])), [0 2]);
asserts.isequal(size(x(1, [])), [1 0]);
x = ones(2, 3, 2);
assert_checkerror('y = x(3, []);', _('Index exceeds array bounds.'));
asserts.isequal(size(x([], 6)), [0 1]);
