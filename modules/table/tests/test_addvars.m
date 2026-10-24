%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
T = table([1; 2], [5; 6], 'VariableNames', {'A', 'C'});
T = addvars(T, [3; 4], 'NewVariableNames', {'B'}, 'Before', 'C');
asserts.isequal(T.Properties.VariableNames, {'A', 'B', 'C'});
asserts.isequal(T.B, [3; 4]);
T = addvars(T, [7; 8], 'NewVariableNames', {'First'}, 'Before', 1);
asserts.isequal(T.Properties.VariableNames, {'First', 'A', 'B', 'C'});
asserts.isequal(T.First, [7; 8]);
%=============================================================================
% timetable input
TT = timetable(seconds([1; 2]), [3; 4]);
R = addvars(TT, [1; 2], 'NewVariableNames', 'x');
asserts.istrue(istimetable(R));
asserts.isequal(R.Properties.VariableNames, {'Var1', 'x'});
asserts.isequal(R.x, [1; 2]);
asserts.isequal(R.Properties.RowTimes, seconds([1; 2]));
R = addvars(TT, [1; 2], 'NewVariableNames', 'x', 'Before', 1);
asserts.isequal(R.Properties.VariableNames, {'x', 'Var1'});
R = addvars(TT, [1; 2], 'NewVariableNames', 'x', 'After', 'Var1');
asserts.isequal(R.Properties.VariableNames, {'Var1', 'x'});
R = addvars(TT, [1; 2], [5; 6]);
asserts.isequal(R.Properties.VariableNames, {'Var1', 'Var2', 'Var3'});
asserts.isequal(R.Var3, [5; 6]);
%=============================================================================
TT = timetable(seconds([1; 2]), [3; 4], 'DimensionNames', {'T', 'V'});
TT.Properties.Description = 'd';
R = addvars(TT, [1; 2]);
asserts.isequal(R.Properties.DimensionNames, {'T', 'V'});
asserts.isequal(R.Properties.Description, 'd');
assert_checkerror('addvars(TT, [1; 2], ''NewVariableNames'', ''T'')', _('Duplicate dimension and variable name: ''T''.'), 'Nelson:timetable:duplicateDimensionVariableNames');
%=============================================================================
