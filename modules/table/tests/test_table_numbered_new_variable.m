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
% a variable number one past the width creates the variable Var<n>
T = table([1; 2], [3; 4], 'VariableNames', {'A', 'B'});
R = T;
R{:, 3} = [7; 8];
assert_isequal(R.Properties.VariableNames, {'A', 'B', 'Var3'});
assert_isequal(R.Var3, [7; 8]);
R = T;
R(:, 3) = table([7; 8]);
assert_isequal(R.Properties.VariableNames, {'A', 'B', 'Var3'});
assert_isequal(R.Var3, [7; 8]);
R = T;
R(:, 3) = {int8(7); int8(8)};
assert_isequal(R.Var3, int8([7; 8]));
%=============================================================================
% on some rows: the other rows get the default value
R = T;
warning('off', 'Nelson:table:rowsAddedNewVars');
R{1, 3} = 5;
assert_isequal(R.Var3, [5; 0]);
R = T;
R(1, 3) = {5};
assert_isequal(R.Var3, [5; 0]);
R = T;
R{1, [3 4]} = [5 6];
warning('on', 'Nelson:table:rowsAddedNewVars');
assert_isequal(R.Properties.VariableNames, {'A', 'B', 'Var3', 'Var4'});
assert_isequal(R.Var4, [6; 0]);
%=============================================================================
% existing and new variables together
R = T;
R{:, [2 3]} = [10 20; 30 40];
assert_isequal(R.B, [10; 30]);
assert_isequal(R.Var3, [20; 40]);
%=============================================================================
% the default name is made unique
R = table(1, 2, 'VariableNames', {'Var3', 'B'});
R{1, 3} = 5;
assert_isequal(R.Properties.VariableNames, {'Var3', 'B', 'Var3_1'});
assert_isequal(R.Var3_1, 5);
%=============================================================================
% a timetable
TT = timetable(seconds([1; 2]), [1; 2], [3; 4], 'VariableNames', {'A', 'B'});
R = TT;
R{:, 3} = [7; 8];
assert_isequal(R.Properties.VariableNames, {'A', 'B', 'Var3'});
assert_isequal(R.Var3, [7; 8]);
assert_isequal(R.Properties.RowTimes, seconds([1; 2]));
%=============================================================================
% a number further out leaves a gap
msg = 'Cannot create a table variable with a discontiguous index.';
assert_checkerror('T{1, 4} = 5;', msg, 'Nelson:table:discontiguousVars');
assert_checkerror('T{:, [3 5]} = [1 2; 3 4];', msg, 'Nelson:table:discontiguousVars');
assert_checkerror('T(1, 5) = T(1, 1);', msg, 'Nelson:table:discontiguousVars');
assert_checkerror('TT{1, 4} = 5;', msg, 'Nelson:table:discontiguousVars');
%=============================================================================
