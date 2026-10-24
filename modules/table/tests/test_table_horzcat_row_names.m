%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% horizontal concatenation takes the row names of the first table that has
% row names, even without rows, and matches the rows of the others by name
TR = table([1; 2], [3; 4], 'VariableNames', {'A', 'B'}, 'RowNames', {'r1', 'r2'});
T0 = table([5; 6], [7; 8], 'VariableNames', {'C', 'D'});
%=============================================================================
k = [TR([], :), T0([], :)];
assert_isequal(size(k), [0 4]);
assert_isequal(size(k.Properties.RowNames), [0 1]);
R = evalc('disp(k)');
assert_istrue(startsWith(R, '        A    B    C    D'));
k = [T0([], :), TR([], :)];
assert_isequal(size(k.Properties.RowNames), [0 1]);
k = [TR([], 1), TR([], 2)];
assert_isequal(size(k.Properties.RowNames), [0 1]);
k = [T0([], 1), T0([], 2)];
assert_isequal(size(k.Properties.RowNames), [0 0]);
%=============================================================================
k = [T0, TR];
assert_isequal(k.Properties.RowNames, {'r1'; 'r2'});
assert_isequal(k.C, [5; 6]);
%=============================================================================
% the rows of the other tables are matched by row name
TS = table([10; 20], 'VariableNames', {'E'}, 'RowNames', {'r2', 'r1'});
k = [TR, TS];
assert_isequal(k.Properties.RowNames, {'r1'; 'r2'});
assert_isequal(k.E, [20; 10]);
k = [TS, TR];
assert_isequal(k.Properties.RowNames, {'r2'; 'r1'});
assert_isequal(k.A, [2; 1]);
%=============================================================================
TX = table([10; 20], 'VariableNames', {'E'}, 'RowNames', {'x1', 'x2'});
assert_checkerror('k = [TR, TX];', _('All tables being horizontally concatenated must have the same row names.'), 'Nelson:table:horzcatRowNamesMismatch');
%=============================================================================
