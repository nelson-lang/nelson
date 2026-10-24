%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% assigning a new variable on some rows fills the other rows with default
% values and warns; T(rows, name) = table or cell adds the variable
%=============================================================================
warnId = 'Nelson:table:rowsAddedNewVars';
T = table([1; 2; 3]);
lastwarn('');
T{1, 'New'} = 5;
[~, id] = lastwarn();
assert_isequal(id, warnId);
assert_isequal(T.New, [5; 0; 0]);
assert_isequal(T.Properties.VariableNames, {'Var1', 'New'});
%=============================================================================
T = table([1; 2; 3]);
T{[1 3], 'New'} = [7; 8];
assert_isequal(T.New, [7; 0; 8]);
T{logical([0 1 0]), 'L'} = true;
assert_isequal(T.L, [false; true; false]);
T{1, 'I'} = int8(4);
assert_isequal(T.I, int8([4; 0; 0]));
T{1, 'C'} = {'q'};
assert_isequal(T.C, {'q'; []; []});
T{2, 'D'} = seconds(4);
assert_isequal(T.D, seconds([0; 4; 0]));
T{1, 'W'} = [1 2];
assert_isequal(T.W, [1 2; 0 0; 0 0]);
T{1, {'N1', 'N2'}} = [5 6];
assert_isequal(T.N1, [5; 0; 0]);
assert_isequal(T.N2, [6; 0; 0]);
T{2, {'Var1', 'N3'}} = [9 10];
assert_isequal(T.Var1, [1; 9; 3]);
assert_isequal(T.N3, [0; 10; 0]);
%=============================================================================
% every row assigned: no warning
T = table([1; 2]);
lastwarn('');
T{[2 1], 'New'} = [7; 8];
[~, id] = lastwarn();
assert_isequal(id, '');
assert_isequal(T.New, [8; 7]);
%=============================================================================
T = table([1; 2], 'RowNames', {'a'; 'b'});
T{'b', 'New'} = 5;
assert_isequal(T.New, [0; 5]);
%=============================================================================
T = table([1; 2; 3]);
msg = _('Value assignment must have the same number of rows as the table.');
assert_checkerror('T{1, ''New''} = [5 6; 7 8];', msg, 'Nelson:table:dimagree:valueAssignmentNumberRowsTable');
assert_isequal(T.Properties.VariableNames, {'Var1'});
%=============================================================================
TT = timetable(seconds([1; 2]), [3; 4]);
TT{1, 'New'} = 5;
assert_isequal(TT.New, [5; 0]);
%=============================================================================
% parentheses
T = table([1; 2]);
T(:, 'NewVar') = table([3; 4]);
assert_isequal(T.Properties.VariableNames, {'Var1', 'NewVar'});
assert_isequal(T.NewVar, [3; 4]);
T(:, {'A', 'B'}) = table([5; 6], {'x'; 'y'});
assert_isequal(T.A, [5; 6]);
assert_isequal(T.B, {'x'; 'y'});
T(:, {'Var1', 'C'}) = table([7; 8], [9; 10]);
assert_isequal(T.Var1, [7; 8]);
assert_isequal(T.C, [9; 10]);
T(1, 'P') = table(11);
assert_isequal(T.P, [11; 0]);
T(:, 'Q') = {3; 4};
assert_isequal(T.Q, [3; 4]);
T(:, 'Var1') = {1; 2};
assert_isequal(T.Var1, [1; 2]);
%=============================================================================
T = table([1; 2]);
msg = _('Right hand side of an assignment into a table must be another table or a cell array.');
assert_checkerror('T(:, ''NewVar'') = [1; 2];', msg, 'Nelson:table:invalidRHS');
assert_checkerror('T(:, ''Var1'') = [1; 2];', msg, 'Nelson:table:invalidRHS');
assert_checkerror('T(:, ''NewVar'') = table([1; 2; 3]);', _('To assign to or create a variable in a table, the number of rows must match the height of the table.'), 'Nelson:table:RowDimensionMismatch');
assert_checkerror('T(:, ''NewVar'') = table([1; 2], [3; 4]);', _('The number of table variables in an assignment must match.'), 'Nelson:table:VarDimensionMismatch');
assert_isequal(T.Properties.VariableNames, {'Var1'});
%=============================================================================
TT = timetable(seconds([1; 2]), [3; 4]);
TT(:, 'NewVar') = table([5; 6]);
assert_isequal(TT.NewVar, [5; 6]);
TT(:, 'S') = timetable(seconds([1; 2]), [7; 8]);
assert_isequal(TT.S, [7; 8]);
assert_checkerror('TT(:, ''X'') = [1; 2];', msg, 'Nelson:table:invalidRHS');
%=============================================================================
