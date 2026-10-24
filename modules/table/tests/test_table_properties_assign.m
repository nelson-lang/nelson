%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite((strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm()), ...
  'Portable runtime table properties assignment scenario triggers a runtime crash.');
T = table([1; 2], [3; 4]);
asserts.checkerror('T.Properties.VariableNames = {''x''};', _('VariableNames must have one name per variable.'));
asserts.checkerror('T.Properties.VariableNames = {''x''};', _('VariableNames must have one name per variable.'), 'Nelson:table:invalidVariableNames');
asserts.checkerror('T.Properties.VariableNames = {''x'', ''y'', ''z''};', _('VariableNames must have one name per variable.'));
asserts.checkerror('T.Properties.VariableNames = {''x'', ''x''};', _('Variable names must be nonempty and distinct.'));
asserts.checkerror('T.Properties.VariableNames = {''x'', ''x''};', _('Variable names must be nonempty and distinct.'), 'Nelson:table:duplicateVariableNames');
asserts.checkerror('T.Properties.VariableNames = {''x'', ''''};', _('Variable names must be nonempty and distinct.'), 'Nelson:table:invalidVariableNames');
asserts.checkerror('T.Properties.VariableNames = {1, 2};', _('Variable names must be nonempty and distinct.'), 'Nelson:table:invalidVariableNames');
asserts.checkerror('T.Properties.VariableUnits = {''m''};', _('VariableUnits must have one element per variable.'));
asserts.checkerror('T.Properties.VariableUnits = {''m''};', _('VariableUnits must have one element per variable.'), 'Nelson:table:invalidVariableProperty');
asserts.checkerror('T.Properties.VariableDescriptions = {''a'', ''b'', ''c''};', _('VariableDescriptions must have one element per variable.'));
asserts.checkerror('T.Properties.VariableContinuity = {''step''};', _('VariableContinuity must have one element per variable.'));
p = T.Properties;
p.VariableNames = {'q'};
asserts.checkerror('T.Properties = p;', _('VariableNames must have one name per variable.'));
asserts.isequal(T.Properties.VariableNames, {'Var1', 'Var2'});
asserts.isequal(T.Properties.VariableUnits, {});
asserts.isequal(T.Var1, [1; 2]);
asserts.isequal(T.Var2, [3; 4]);
R = T(2, :);
asserts.isequal(R.Var2, 4);
%=============================================================================
T = table([1; 2], [3; 4]);
T.Properties.VariableNames = {'Var2', 'Var1'};
asserts.isequal(T.Var2, [1; 2]);
asserts.isequal(T.Var1, [3; 4]);
asserts.isequal(T{:, 'Var1'}, [3; 4]);
%=============================================================================
T = table([1; 2], [3; 4]);
T.Properties.VariableNames = ["a", "b"];
asserts.isequal(T.Properties.VariableNames, {'a', 'b'});
asserts.isequal(T.a, [1; 2]);
asserts.isequal(T.b, [3; 4]);
T.Properties.VariableNames{2} = 'c';
asserts.isequal(T.c, [3; 4]);
T.z = [5; 6];
asserts.isequal(T.Properties.VariableNames, {'a', 'c', 'z'});
asserts.isequal(T.z, [5; 6]);
%=============================================================================
T = table([1; 2], [3; 4]);
T.Properties.VariableUnits = {'m', 's'};
T.Properties.VariableDescriptions = {'first', 'second'};
R = T(1, :);
asserts.isequal(R.Properties.VariableUnits, {'m', 's'});
asserts.isequal(R.Properties.VariableDescriptions, {'first', 'second'});
T.Properties.VariableUnits = {};
asserts.isequal(T.Properties.VariableUnits, {});
%=============================================================================
T = table([1; 2], [3; 4]);
p = T.Properties;
p.VariableNames = {'x', 'y'};
p.Description = 'renamed';
T.Properties = p;
asserts.isequal(T.x, [1; 2]);
asserts.isequal(T.y, [3; 4]);
asserts.isequal(T.Properties.Description, 'renamed');
%=============================================================================
T = table([1; 2], [3; 4]);
asserts.checkerror('T.Properties = 3;', _('Properties must be a scalar structure.'));
asserts.checkerror('T.Properties = 3;', _('Properties must be a scalar structure.'), 'Nelson:table:invalidPropertiesAssignment');
p = T.Properties;
p.Bogus = 1;
asserts.checkerror('T.Properties = p;', sprintf(_('Unknown table property: %s.'), 'Bogus'));
asserts.checkerror('T.Properties = p;', sprintf(_('Unknown table property: %s.'), 'Bogus'), 'Nelson:table:unknownProperty');
asserts.isfalse(isfield(T.Properties, 'Bogus'));
%=============================================================================
T = table([1; 2], [3; 4]);
T.Properties.VariableUnits = {'m', 's'};
p = rmfield(T.Properties, 'VariableUnits');
p.Description = 'kept units';
T.Properties = p;
asserts.isequal(T.Properties.VariableUnits, {'m', 's'});
asserts.isequal(T.Properties.Description, 'kept units');
%=============================================================================
filename = [tempname(), '.csv'];
writematrix([1 2; 3 4], filename);
T = readtable(filename);
delete(filename);
T.Properties.VariableUnits = {'m', 's'};
T.Properties.VariableNames = {'a', 'b'};
asserts.isequal(T.b, [2; 4]);
asserts.isequal(T.Properties.VariableUnits, {'m', 's'});
asserts.checkerror('T.Properties.VariableNames = {''a''};', _('VariableNames must have one name per variable.'));
%=============================================================================
msg = _('To assign multiple variable names, specify nonempty names in a string array or a cell array of character vectors.');
T = table([1; 2]);
asserts.checkerror('T.Properties.VariableNames = ''z'';', msg, 'Nelson:table:invalidVariableNames');
asserts.checkerror('T.Properties.VariableNames = 5;', msg, 'Nelson:table:invalidVariableNames');
p = T.Properties;
p.VariableNames = 'z';
asserts.checkerror('T.Properties = p;', msg, 'Nelson:table:invalidVariableNames');
asserts.isequal(T.Properties.VariableNames, {'Var1'});
T.Properties.VariableNames = "z";
asserts.isequal(T.Properties.VariableNames, {'z'});
T = renamevars(T, 'z', 'w');
asserts.isequal(T.Properties.VariableNames, {'w'});
T = table([1; 2], [3; 4]);
asserts.checkerror('T.Properties.VariableNames = ''z'';', msg, 'Nelson:table:invalidVariableNames');
%=============================================================================
