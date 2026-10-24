%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% a variable named like an internal field is reached by dot indexing
%=============================================================================
TT = timetable(seconds([1; 2]), [5; 6], 'VariableNames', {'Data'});
assert_isequal(TT.Data, [5; 6]);
assert_isequal(TT.Data(1), 5);
TT.Data = [7; 8];
assert_isequal(TT.Data, [7; 8]);
TT.Data(2) = 9;
assert_isequal(TT.Data, [7; 9]);
assert_isequal(TT.Properties.VariableNames, {'Data'});
%=============================================================================
TT = timetable(seconds([1; 2]), [5; 6]);
TT.Data = [7; 8];
assert_isequal(TT.Properties.VariableNames, {'Var1', 'Data'});
assert_isequal(TT.Data, [7; 8]);
assert_isequal(TT.Var1, [5; 6]);
%=============================================================================
TT = timetable(seconds([1; 2]), [5; 6]);
TT.Properties.VariableNames = {'Data'};
assert_isequal(TT.Data, [5; 6]);
%=============================================================================
TT = timetable(seconds([1; 2]), [5; 6], 'VariableNames', {'data'});
assert_isequal(TT.data, [5; 6]);
TT.data(1) = 3;
assert_isequal(TT.data, [3; 6]);
%=============================================================================
T = table([5; 6], 'VariableNames', {'data'});
assert_isequal(T.data, [5; 6]);
assert_isequal(size(T), [2 1]);
T.data = [1; 2];
assert_isequal(T.data, [1; 2]);
T.data(1) = 9;
assert_isequal(T.data, [9; 2]);
assert_isequal(T.Properties.VariableTypes, "double");
%=============================================================================
T = table([5; 6], [1; 2], 'VariableNames', {'Version', 'Data'});
assert_isequal(T.Version, [5; 6]);
assert_isequal(T.Data, [1; 2]);
%=============================================================================
% names reserved by the table interface cannot name a variable
%=============================================================================
reserved = {'Properties', 'RowNames', 'VariableNames', ':'};
for k = 1:length(reserved)
  msg = sprintf(_('Table variable name conflicts with reserved name ''%s''.'), reserved{k});
  cmd = sprintf('table([5; 6], ''VariableNames'', {''%s''})', reserved{k});
  assert_checkerror(cmd, msg, 'Nelson:table:reservedVariableNameConflict');
  cmd = sprintf('timetable(seconds([1; 2]), [5; 6], ''VariableNames'', {''%s''})', reserved{k});
  assert_checkerror(cmd, msg, 'Nelson:timetable:reservedVariableNameConflict');
end
%=============================================================================
msg = sprintf(_('Table variable name conflicts with reserved name ''%s''.'), 'Properties');
T = table([5; 6]);
assert_checkerror('T.Properties.VariableNames = {''Properties''};', msg, 'Nelson:table:reservedVariableNameConflict');
assert_checkerror('renamevars(T, ''Var1'', ''Properties'');', msg, 'Nelson:table:reservedVariableNameConflict');
Properties = [1; 2];
assert_checkerror('table(Properties);', msg, 'Nelson:table:reservedVariableNameConflict');
TT = timetable(seconds([1; 2]), [5; 6]);
assert_checkerror('TT.Properties.VariableNames = {''Properties''};', msg, 'Nelson:timetable:reservedVariableNameConflict');
msg = sprintf(_('Table variable name conflicts with reserved name ''%s''.'), 'RowNames');
assert_checkerror('addvars(T, [1; 2], ''NewVariableNames'', ''RowNames'');', msg, 'Nelson:table:reservedVariableNameConflict');
%=============================================================================
T = table([5; 6], 'VariableNames', {'RowTimes'});
assert_isequal(T.RowTimes, [5; 6]);
assert_isequal(properties(T), {'RowTimes'; 'Properties'; 'Row'; 'Variables'});
%=============================================================================
