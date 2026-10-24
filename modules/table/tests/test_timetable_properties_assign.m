%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
TT = timetable(seconds([1; 2]), [1; 2], [3; 4]);
TT.Properties.VariableNames = {'x', 'y'};
assert_isequal(TT.Properties.VariableNames, {'x', 'y'});
assert_isequal(TT.x, [1; 2]);
assert_isequal(TT.y, [3; 4]);
assert_isequal(TT{:, 'y'}, [3; 4]);
R = TT(2, :);
assert_isequal(R.Properties.VariableNames, {'x', 'y'});
assert_isequal(R.x, 2);
%=============================================================================
TT = timetable(seconds([1; 2]), [1; 2], [3; 4]);
p = TT.Properties;
p.VariableNames = {'x', 'y'};
TT.Properties = p;
assert_isequal(TT.x, [1; 2]);
assert_isequal(TT.y, [3; 4]);
%=============================================================================
TT = timetable(seconds([1; 2]), [1; 2], [3; 4]);
TT.Properties.VariableNames = {'Var2', 'Var1'};
assert_isequal(TT.Var2, [1; 2]);
assert_isequal(TT.Var1, [3; 4]);
%=============================================================================
TT = timetable(seconds([1; 2]), [1; 2], [3; 4]);
TT.Properties.VariableUnits = {'m', 's'};
assert_isequal(TT.Var1, [1; 2]);
assert_isequal(TT.Properties.VariableUnits, {'m', 's'});
R = TT(1, :);
assert_isequal(R.Properties.VariableUnits, {'m', 's'});
%=============================================================================
TT = timetable(seconds([1; 2]), [1; 2], [3; 4]);
TT.Properties.VariableDescriptions = {'a', 'b'};
R = TT(1, :);
assert_isequal(R.Properties.VariableDescriptions, {'a', 'b'});
%=============================================================================
TT = timetable(seconds([1; 2]), [1; 2], [3; 4]);
TT.Properties.Description = 'hello';
R = TT(1, :);
assert_isequal(R.Properties.Description, 'hello');
%=============================================================================
TT = timetable(seconds([1; 2]), [1; 2], [3; 4]);
TT.Properties.VariableNames = {'x', 'y'};
TT.z = [5; 6];
assert_isequal(TT.Properties.VariableNames, {'x', 'y', 'z'});
assert_isequal(TT.z, [5; 6]);
%=============================================================================
TT = timetable(seconds([1; 2]), [1; 2], [3; 4]);
assert_checkerror('TT.Properties.VariableNames = {''x'', ''x''};', _('Variable names must be nonempty and distinct.'));
assert_checkerror('TT.Properties.VariableNames = {''x''};', _('VariableNames must have one name per variable.'));
assert_checkerror('TT.Properties.VariableUnits = {''m''};', _('VariableUnits must have one element per variable.'));
assert_checkerror('TT.Properties.VariableDescriptions = {''a'', ''b'', ''c''};', _('VariableDescriptions must have one element per variable.'));
assert_isequal(TT.Properties.VariableNames, {'Var1', 'Var2'});
assert_isequal(TT.Var1, [1; 2]);
%=============================================================================
TT = timetable(seconds([1; 2]), [1; 2], [3; 4]);
R = renamevars(TT, 'Var1', 'z');
assert_istrue(istimetable(R));
assert_isequal(R.Properties.VariableNames, {'z', 'Var2'});
assert_isequal(R.z, [1; 2]);
assert_isequal(R.Properties.RowTimes, seconds([1; 2]));
R = renamevars(TT, {'Var1', 'Var2'}, {'a', 'b'});
assert_isequal(R.b, [3; 4]);
assert_checkerror('renamevars(TT, ''nope'', ''z'')', _('Name "nope" not found in the table.'));
%=============================================================================
TT = timetable(seconds([1; 2]), [1; 2], [3; 4], 'VariableUnits', {'m', 's'}, 'VariableDescriptions', {'a', 'b'});
assert_isequal(TT.Properties.VariableUnits, {'m', 's'});
R = TT(1, :);
assert_isequal(R.Properties.VariableDescriptions, {'a', 'b'});
%=============================================================================
TT = timetable(seconds([1; 2]), [1; 2], [3; 4]);
asserts.checkerror('TT.Properties = 3;', _('Properties must be a scalar structure.'));
asserts.checkerror('TT.Properties = 3;', _('Properties must be a scalar structure.'), 'Nelson:table:invalidPropertiesAssignment');
p = TT.Properties;
p.Bogus = 1;
asserts.checkerror('TT.Properties = p;', sprintf(_('Unknown table property: %s.'), 'Bogus'), 'Nelson:table:unknownProperty');
asserts.isfalse(isfield(TT.Properties, 'Bogus'));
%=============================================================================
TT = timetable(seconds([1; 2]), [1; 2], [3; 4]);
TT.Properties.VariableUnits = {'m', 's'};
p = rmfield(TT.Properties, 'VariableUnits');
p.VariableNames = {'x', 'y'};
TT.Properties = p;
asserts.isequal(TT.Properties.VariableUnits, {'m', 's'});
asserts.isequal(TT.y, [3; 4]);
R = TT(1, :);
asserts.isequal(R.Properties.VariableUnits, {'m', 's'});
%=============================================================================
msg = _('To assign multiple variable names, specify nonempty names in a string array or a cell array of character vectors.');
TT = timetable(seconds([1; 2]), [1; 2]);
asserts.checkerror('TT.Properties.VariableNames = ''z'';', msg, 'Nelson:table:invalidVariableNames');
asserts.isequal(TT.Properties.VariableNames, {'Var1'});
TT.Properties.VariableNames = "z";
asserts.isequal(TT.Properties.VariableNames, {'z'});
TT = renamevars(TT, 'z', 'w');
asserts.isequal(TT.Properties.VariableNames, {'w'});
%=============================================================================
