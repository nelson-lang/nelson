%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% T{:, name} = value creates the variable when it does not exist
%=============================================================================
T = table([1; 2]);
T{:, 'NewVar'} = [5; 6];
assert_isequal(T.Properties.VariableNames, {'Var1', 'NewVar'});
assert_isequal(T.NewVar, [5; 6]);
%=============================================================================
T = table([1; 2]);
T{:, "NewVar"} = [5; 6];
assert_isequal(T.NewVar, [5; 6]);
%=============================================================================
T = table([1; 2]);
T{:, {'A', 'B'}} = [5 6; 7 8];
assert_isequal(T.Properties.VariableNames, {'Var1', 'A', 'B'});
assert_isequal(T.A, [5; 7]);
assert_isequal(T.B, [6; 8]);
%=============================================================================
T = table([1; 2]);
T{:, 'NewVar'} = 5;
assert_isequal(T.NewVar, [5; 5]);
T{:, 'M'} = [5 6; 7 8];
assert_isequal(T.M, [5 6; 7 8]);
T{:, 'C'} = {'a'; 'b'};
assert_isequal(T.C, {'a'; 'b'});
assert_isequal(size(T), [2 4]);
%=============================================================================
% existing and new variables in one assignment
T = table([1; 2]);
T{:, {'Var1', 'B'}} = [3 4; 5 6];
assert_isequal(T.Var1, [3; 5]);
assert_isequal(T.B, [4; 6]);
%=============================================================================
T = table();
T{:, 'x'} = [1; 2];
assert_isequal(size(T), [2 1]);
assert_isequal(T.x, [1; 2]);
%=============================================================================
T = table([1; 2]);
T(:, 1) = [];
T{:, 'x'} = [1; 2];
assert_isequal(size(T), [2 1]);
%=============================================================================
TT = timetable(seconds([1; 2]), [3; 4]);
TT{:, 'NewVar'} = [5; 6];
assert_isequal(TT.Properties.VariableNames, {'Var1', 'NewVar'});
assert_isequal(TT.NewVar, [5; 6]);
TT{:, "S"} = [7; 8];
assert_isequal(TT.S, [7; 8]);
%=============================================================================
T = table([1; 2]);
assert_checkerror('T{:, ''NewVar''} = [5; 6; 7];', _('Value assignment must have the same number of rows as the table.'), 'Nelson:table:dimagree:valueAssignmentNumberRowsTable');
assert_checkerror('T{:, ''RowNames''} = [5; 6];', _('Table variable name conflicts with reserved name ''RowNames''.'), 'Nelson:table:reservedVariableNameConflict');
assert_isequal(T.Properties.VariableNames, {'Var1'});
%=============================================================================
