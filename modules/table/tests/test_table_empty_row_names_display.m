%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% a table with row names and no rows keeps the (empty) row names column in
% its display: the header is indented
TR = table([1; 2], [3; 4], 'VariableNames', {'A', 'B'}, 'RowNames', {'r1', 'r2'});
T = TR([], :);
assert_isequal(size(T.Properties.RowNames), [0 1]);
R = evalc('disp(T)');
assert_istrue(startsWith(R, ['        A    B', newline]) || startsWith(R, ['        A    B    ', newline]));
R = evalc('T');
assert_istrue(contains(R, [newline, '        A    B']));
%=============================================================================
% deleting every row keeps the row names
T = TR;
T([1 2], :) = [];
assert_isequal(size(T.Properties.RowNames), [0 1]);
R = evalc('disp(T)');
assert_istrue(startsWith(R, '        A    B'));
%=============================================================================
% without row names, or after removing them, no indent
T0 = table([1; 2], [3; 4], 'VariableNames', {'A', 'B'});
T = T0([], :);
R = evalc('disp(T)');
assert_istrue(startsWith(R, '    A    B'));
T = TR([], :);
T.Properties.RowNames = {};
assert_isequal(size(T.Properties.RowNames), [0 0]);
R = evalc('disp(T)');
assert_istrue(startsWith(R, '    A    B'));
T = table.empty(0, 2);
R = evalc('disp(T)');
assert_istrue(startsWith(R, '    Var1    Var2'));
%=============================================================================
% a timetable shows its time column
TT = timetable(seconds([1; 2]), [1; 2], [3; 4], 'VariableNames', {'A', 'B'});
T = TT([], :);
R = evalc('disp(T)');
assert_istrue(startsWith(R, '    Time    A    B'));
%=============================================================================
