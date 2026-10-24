%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
% A bracketed cell-index list `[x{1:N}]` fed from a comma-separated-list
% right-hand side expands to one assignment target per index: each value of
% the list is written to x{i}, growing x as needed.
%=============================================================================
c = {10, 20, 30};
[x{1:3}] = c{:};
assert_isequal(x, {10, 20, 30});
%=============================================================================
% The target is grown from an undefined variable.
%=============================================================================
clear x
c = {10, 20, 30};
[x{1:3}] = c{:};
assert_isequal(numel(x), 3);
assert_isequal(x{1}, 10);
assert_isequal(x{2}, 20);
assert_isequal(x{3}, 30);
%=============================================================================
% Non-contiguous target indices leave the gaps as empty cells.
%=============================================================================
clear x
c = {10, 20, 30};
[x{[1 3]}] = c{1:2};
assert_isequal(numel(x), 3);
assert_isequal(x{1}, 10);
assert_isequal(x{2}, []);
assert_isequal(x{3}, 20);
%=============================================================================
% Permuted target indices place values at their own positions.
%=============================================================================
clear x
[x{[2 1]}] = deal(10, 20);
assert_isequal(x{1}, 20);
assert_isequal(x{2}, 10);
%=============================================================================
% A right-hand side longer than the target count fills the first N targets.
%=============================================================================
clear x
c = {10, 20, 30};
[x{1:2}] = c{:};
assert_isequal(numel(x), 2);
assert_isequal(x{1}, 10);
assert_isequal(x{2}, 20);
%=============================================================================
% A struct-array field comma-separated-list is a valid source.
%=============================================================================
clear s x
s(1).f = 1;
s(2).f = 2;
[x{1:2}] = s.f;
assert_isequal(x, {1, 2});
%=============================================================================
% An indexed slice of a cell is a valid source.
%=============================================================================
clear x
c = {1, 2, 3};
[x{1:2}] = c{2:3};
assert_isequal(x, {2, 3});
%=============================================================================
% Existing entries outside the target range are preserved.
%=============================================================================
clear x
c = {10, 20, 30};
x = {7, 8, 9, 10, 11};
[x{2:4}] = c{:};
assert_isequal(x, {7, 10, 20, 30, 11});
%=============================================================================
% A function right-hand side keeps working.
%=============================================================================
clear x
[x{1:2}] = deal(5, 6);
assert_isequal(x, {5, 6});
%=============================================================================
% An empty cell target grows correctly.
%=============================================================================
x = {};
[x{1:2}] = deal(5, 6);
assert_isequal(x, {5, 6});
%=============================================================================
% Too few right-hand side values is an error.
%=============================================================================
clear x
c = {10, 20, 30};
assert_checkerror('[x{1:3}] = c{1:2};', _('Insufficient number of outputs from right hand side of equal sign to satisfy assignment.'));
%=============================================================================
% Named-variable targets keep distributing a comma-separated-list.
%=============================================================================
c = {10, 20, 30};
[a, b, d] = c{:};
assert_isequal(a, 10);
assert_isequal(b, 20);
assert_isequal(d, 30);
%=============================================================================
% A multi-subscript brace target `[m{1:2,1}]` grows a column cell; the target
% count is the product of the per-subscript element counts and values are
% placed in column-major order.
%=============================================================================
clear m
m = {};
[m{1:2, 1}] = deal(7, 8);
assert_isequal(m, {7; 8});
%=============================================================================
% A multi-subscript brace target along the second dimension.
%=============================================================================
clear m
m = {};
[m{1, 1:2}] = deal(7, 8);
assert_isequal(m, {7, 8});
%=============================================================================
% A 2x2 multi-subscript target fills column-major.
%=============================================================================
clear m
m = {};
[m{1:2, 1:2}] = deal(1, 2, 3, 4);
assert_isequal(m, {1, 3; 2, 4});
%=============================================================================
% A comma-separated-list right-hand side feeds a multi-subscript target.
%=============================================================================
clear m
c = {10, 20, 30, 40};
[m{1:2, 1:2}] = c{:};
assert_isequal(m, {10, 30; 20, 40});
%=============================================================================
% Non-contiguous multi-subscript indices leave the gaps as empty cells.
%=============================================================================
clear m
[m{[1 3], 1}] = deal(7, 8);
assert_isequal(m, {7; []; 8});
%=============================================================================
% A colon subscript spans the existing dimension of the target.
%=============================================================================
clear m
m = cell(2, 3);
[m{:, 2}] = deal(10, 20);
assert_isequal(m{1, 2}, 10);
assert_isequal(m{2, 2}, 20);
%=============================================================================
% An `end` subscript resolves against the current target size.
%=============================================================================
clear m
m = cell(3, 2);
[m{1:end, 1}] = deal(1, 2, 3);
assert_isequal(m{1, 1}, 1);
assert_isequal(m{2, 1}, 2);
assert_isequal(m{3, 1}, 3);
%=============================================================================
% Entries outside a multi-subscript target range are preserved.
%=============================================================================
clear m
m = cell(3, 3);
m{1, 1} = 99;
[m{2:3, 2}] = deal(5, 6);
assert_isequal(m{1, 1}, 99);
assert_isequal(m{2, 2}, 5);
assert_isequal(m{3, 2}, 6);
%=============================================================================
% Too few right-hand side values for a multi-subscript target is an error.
%=============================================================================
clear m
c = {1, 2};
assert_checkerror('[m{1:2,1:2}] = c{:};', _('Insufficient number of outputs from right hand side of equal sign to satisfy assignment.'));
%=============================================================================
