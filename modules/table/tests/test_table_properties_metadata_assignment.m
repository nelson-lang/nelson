%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Description and UserData edits through Properties
%=============================================================================
T = table([1; 2], [3; 4]);
T.Properties.Description = 'x';
assert_isequal(T.Properties.Description, 'x');
T.Properties.UserData = {1, 'a'};
assert_isequal(T.Properties.UserData, {1, 'a'});
assert_isequal(T.Var2, [3; 4]);
%=============================================================================
% a whole Properties value with other changes is still checked and applied
P = T.Properties;
P.Description = 'y';
P.VariableNames = {'a', 'b'};
T.Properties = P;
assert_isequal(T.Properties.Description, 'y');
assert_isequal(T.a, [1; 2]);
P = T.Properties;
P.Description = 'z';
P.RowNames = {'r1'};
assert_checkerror('T.Properties = P;', _('The RowNames property must contain one name for each row in the table.'), 'Nelson:table:incorrectNumberOfRowNames');
assert_isequal(T.Properties.Description, 'y');
P = rmfield(T.Properties, 'VariableUnits');
P.Description = 'w';
T.Properties = P;
assert_isequal(T.Properties.Description, 'w');
assert_istrue(isfield(T.Properties, 'VariableUnits'));
%=============================================================================
TT = timetable(seconds([1; 2]), [3; 4]);
TT.Properties.Description = 'x';
assert_isequal(TT.Properties.Description, 'x');
TT.Properties.UserData = 5;
assert_isequal(TT.Properties.UserData, 5);
assert_isequal(TT.Var1, [3; 4]);
P = TT.Properties;
P.Description = 'y';
P.VariableNames = {'v'};
TT.Properties = P;
assert_isequal(TT.Properties.Description, 'y');
assert_isequal(TT.v, [3; 4]);
P = TT.Properties;
P.RowTimes = seconds([5; 6]);
TT.Properties = P;
assert_isequal(TT.Properties.StartTime, seconds(5));
%=============================================================================
