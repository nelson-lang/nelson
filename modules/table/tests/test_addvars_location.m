%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
T = table([1; 2], [3; 4], [5; 6], 'VariableNames', {'A', 'B', 'C'});
x = [7; 8];
%=============================================================================
% location by name, index, logical mask; 'After', 0 and 'Before', width + 1
R = addvars(T, x, 'Before', 'B');
assert_isequal(R.Properties.VariableNames, {'A', 'x', 'B', 'C'});
assert_isequal(R.x, x);
R = addvars(T, x, 'After', 2);
assert_isequal(R.Properties.VariableNames, {'A', 'B', 'x', 'C'});
R = addvars(T, x, 'After', 0);
assert_isequal(R.Properties.VariableNames, {'x', 'A', 'B', 'C'});
R = addvars(T, x, 'Before', 4);
assert_isequal(R.Properties.VariableNames, {'A', 'B', 'C', 'x'});
R = addvars(T, x, 'Before', logical([0 1 0]));
assert_isequal(R.Properties.VariableNames, {'A', 'x', 'B', 'C'});
R = addvars(T, x, 'After', logical([0 1 0]));
assert_isequal(R.Properties.VariableNames, {'A', 'B', 'x', 'C'});
R = addvars(T, x, 'Before', true);
assert_isequal(R.Properties.VariableNames, {'x', 'A', 'B', 'C'});
R = addvars(T, x, 'After', int8(1));
assert_isequal(R.Properties.VariableNames, {'A', 'x', 'B', 'C'});
R = addvars(T, x, 'Before', {'B'});
assert_isequal(R.Properties.VariableNames, {'A', 'x', 'B', 'C'});
R = addvars(T, x, 'Before', "B");
assert_isequal(R.Properties.VariableNames, {'A', 'x', 'B', 'C'});
R = addvars(T);
assert_isequal(R.Properties.VariableNames, {'A', 'B', 'C'});
%=============================================================================
% name-value names: case-insensitive, abbreviable, last occurrence wins
R = addvars(T, x, 'b', 'B');
assert_isequal(R.Properties.VariableNames, {'A', 'x', 'B', 'C'});
R = addvars(T, x, 'aft', 'B');
assert_isequal(R.Properties.VariableNames, {'A', 'B', 'x', 'C'});
R = addvars(T, x, 'N', 'Z');
assert_isequal(R.Properties.VariableNames, {'A', 'B', 'C', 'Z'});
R = addvars(T, x, 'Before', 'B', 'Before', 'C');
assert_isequal(R.Properties.VariableNames, {'A', 'B', 'x', 'C'});
R = addvars(T, x, 'Before', 'B', 'NewVariableNames', 'Z', 'Before', 'A');
assert_isequal(R.Properties.VariableNames, {'Z', 'A', 'B', 'C'});
R = addvars(T, x, 'NewVariableNames', 'Z', 'NewVariableNames', 'W');
assert_isequal(R.Properties.VariableNames, {'A', 'B', 'C', 'W'});
R = addvars(T, 'NewVariableNames', {});
assert_isequal(R.Properties.VariableNames, {'A', 'B', 'C'});
%=============================================================================
% default names skip the existing ones
T2 = table([1; 2], [3; 4], 'VariableNames', {'Var3', 'x'});
R = addvars(T2, [1; 2], [3; 4]);
assert_isequal(R.Properties.VariableNames, {'Var3', 'x', 'Var3_1', 'Var4'});
%=============================================================================
% timetable and eventtable
TT = timetable(seconds([1; 2]), [3; 4], [5; 6], 'VariableNames', {'A', 'B'});
R = addvars(TT, x, 'After', 0);
assert_istrue(istimetable(R));
assert_isequal(R.Properties.VariableNames, {'x', 'A', 'B'});
R = addvars(TT, x, 'Before', 3);
assert_isequal(R.Properties.VariableNames, {'A', 'B', 'x'});
R = addvars(TT, x, 'Before', logical([0 1]));
assert_isequal(R.Properties.VariableNames, {'A', 'x', 'B'});
ET = eventtable(seconds([1; 2]), 'EventLabels', ["a"; "b"]);
R = addvars(ET, x, 'After', 0);
assert_isequal(class(R), 'eventtable');
assert_isequal(R.Properties.VariableNames, {'x', 'EventLabels'});
%=============================================================================
% errors
msg = _('Specify a single location as a positive integer, a variable name, or a logical vector containing a single true value.');
assert_checkerror('addvars(T, x, ''Before'', [1 2])', msg, 'Nelson:table:nonscalarPosition');
assert_checkerror('addvars(T, x, ''Before'', logical([0 1 1]))', msg, 'Nelson:table:nonscalarPosition');
assert_checkerror('addvars(T, x, ''Before'', {''A'', ''B''})', msg, 'Nelson:table:nonscalarPosition');
assert_checkerror('addvars(T, x, ''Before'', [])', msg, 'Nelson:table:nonscalarPosition');
msg = _('Variable index exceeds table dimensions.');
assert_checkerror('addvars(T, x, ''After'', 4)', msg, 'Nelson:table:varIndexOutOfRange');
assert_checkerror('addvars(T, x, ''Before'', 5)', msg, 'Nelson:table:varIndexOutOfRange');
assert_checkerror('addvars(T, x, ''Before'', logical([0 0 0 1]))', msg, 'Nelson:table:varIndexOutOfRange');
assert_checkerror('addvars(TT, x, ''Before'', 4)', msg, 'Nelson:table:varIndexOutOfRange');
msg = _('index must either be real positive integers or logicals.');
assert_checkerror('addvars(T, x, ''Before'', 0)', msg, 'Nelson:types:invalidIndex:indexEitherRealPositiveIntegersLogicals');
assert_checkerror('addvars(T, x, ''Before'', 1.5)', msg, 'Nelson:types:invalidIndex:indexEitherRealPositiveIntegersLogicals');
msg = _('Using a vartype subscripter as a location is not supported. Specify a location as a variable name, a positive integer, or a logical vector containing a single true value.');
assert_checkerror('addvars(T, x, ''Before'', vartype(''double''))', msg, 'Nelson:table:invalidLocation');
msg = _('Specify either ''Before'' or ''After'', but not both.');
assert_checkerror('addvars(T, x, ''Before'', ''B'', ''After'', ''A'')', msg, 'Nelson:table:beforeAndAfter');
msg = _('Incorrect number of arguments.');
assert_checkerror('addvars(T, x, ''Before'')', msg, 'Nelson:table:wrongNumberArgs');
assert_checkerror('addvars(T, x, ''B'')', msg, 'Nelson:table:wrongNumberArgs');
assert_checkerror('addvars(T, x, ''Before'', ''B'', 5)', msg, 'Nelson:table:wrongNumberArgs');
assert_checkerror('addvars(T, x, ''Foo'', 1)', sprintf(_('Invalid parameter name: %s.'), 'Foo'), 'Nelson:table:badParamName');
assert_checkerror('addvars(T, x, '''', 1)', _('Invalid parameter name. Parameter name must be a nonempty string or character vector.'), 'Nelson:table:illegalParamName');
msg = _('Specify one new variable name for each variable being added to the table.');
assert_checkerror('addvars(T, x, ''NewVariableNames'', {''Z'', ''W''})', msg, 'Nelson:table:incorrectNumberOfVarNames');
assert_checkerror('addvars(T, x, x, ''NewVariableNames'', {''Z''})', msg, 'Nelson:table:incorrectNumberOfVarNames');
msg = _('New variable names must be a character vector, string array, or cell array of character vectors.');
assert_checkerror('addvars(T, x, ''NewVariableNames'', 5)', msg, 'Nelson:table:invalidVarNames');
assert_checkerror('addvars(T, x, ''NewVariableNames'', {5})', msg, 'Nelson:table:invalidVarNames');
assert_checkerror('addvars(T, x, ''NewVariableNames'', '''')', _('Table variable names must be nonempty text.'), 'Nelson:table:zeroLengthVarname');
msg = sprintf(_('Duplicate table variable name: ''%s''.'), 'Z');
assert_checkerror('addvars(T, x, x, ''NewVariableNames'', {''Z'', ''Z''})', msg, 'Nelson:table:duplicateVariableNames');
msg = sprintf(_('Duplicate table variable name: ''%s''.'), 'A');
assert_checkerror('addvars(T, x, ''NewVariableNames'', ''A'')', msg, 'Nelson:table:duplicateVariableNames');
%=============================================================================
