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
% The variables to move and the Before/After location may be names, numeric
% indices or logical masks.
T = table([1; 2], [3; 4], [5; 6], [7; 8], 'VariableNames', {'A', 'B', 'C', 'D'});
T.Properties.VariableUnits = {'ua', 'ub', 'uc', 'ud'};
R = movevars(T, 1, 'After', 2);
asserts.isequal(R.Properties.VariableNames, {'B', 'A', 'C', 'D'});
asserts.isequal(R.Properties.VariableUnits, {'ub', 'ua', 'uc', 'ud'});
asserts.isequal(R.A, [1; 2]);
asserts.isequal(R.B, [3; 4]);
R = movevars(T, [1 3], 'After', 4);
asserts.isequal(R.Properties.VariableNames, {'B', 'D', 'A', 'C'});
R = movevars(T, [3 1], 'After', 4);
asserts.isequal(R.Properties.VariableNames, {'B', 'D', 'C', 'A'});
R = movevars(T, [3; 1], 'Before', 1);
asserts.isequal(R.Properties.VariableNames, {'C', 'A', 'B', 'D'});
R = movevars(T, int8([1 3]), 'After', 4);
asserts.isequal(R.Properties.VariableNames, {'B', 'D', 'A', 'C'});
R = movevars(T, logical([1 0 1 0]), 'Before', 2);
asserts.isequal(R.Properties.VariableNames, {'A', 'C', 'B', 'D'});
R = movevars(T, logical([1 0 1]), 'Before', 2);
asserts.isequal(R.Properties.VariableNames, {'A', 'C', 'B', 'D'});
R = movevars(T, logical([0 0 0 1 0]), 'Before', 1);
asserts.isequal(R.Properties.VariableNames, {'D', 'A', 'B', 'C'});
R = movevars(T, true, 'After', 2);
asserts.isequal(R.Properties.VariableNames, {'B', 'A', 'C', 'D'});
R = movevars(T, 'A', 'After', logical([0 0 1 0]));
asserts.isequal(R.Properties.VariableNames, {'B', 'C', 'A', 'D'});
R = movevars(T, 'A', 'After', int8(3));
asserts.isequal(R.Properties.VariableNames, {'B', 'C', 'A', 'D'});
R = movevars(T, 'A', 'After', {'B'});
asserts.isequal(R.Properties.VariableNames, {'B', 'A', 'C', 'D'});
R = movevars(T, 'A', 'After', "B");
asserts.isequal(R.Properties.VariableNames, {'B', 'A', 'C', 'D'});
R = movevars(T, 1, 'After', 'D');
asserts.isequal(R.Properties.VariableNames, {'B', 'C', 'D', 'A'});
R = movevars(T, ["A", "C"], 'After', 4);
asserts.isequal(R.Properties.VariableNames, {'B', 'D', 'A', 'C'});
R = movevars(T, {'A'; 'C'}, 'After', 4);
asserts.isequal(R.Properties.VariableNames, {'B', 'D', 'A', 'C'});
%=============================================================================
% 'After', 0 is the first position and 'Before', width + 1 the last one.
R = movevars(T, 3, 'After', 0);
asserts.isequal(R.Properties.VariableNames, {'C', 'A', 'B', 'D'});
R = movevars(T, 1, 'Before', 5);
asserts.isequal(R.Properties.VariableNames, {'B', 'C', 'D', 'A'});
%=============================================================================
% The location refers to the input table, moved variables included.
R = movevars(T, [1 3], 'After', 1);
asserts.isequal(R.Properties.VariableNames, {'A', 'C', 'B', 'D'});
R = movevars(T, [1 3], 'Before', 3);
asserts.isequal(R.Properties.VariableNames, {'B', 'A', 'C', 'D'});
R = movevars(T, [2 4], 'After', 2);
asserts.isequal(R.Properties.VariableNames, {'A', 'B', 'D', 'C'});
R = movevars(T, [4 2], 'After', 2);
asserts.isequal(R.Properties.VariableNames, {'A', 'D', 'B', 'C'});
R = movevars(T, [2 4], 'Before', 4);
asserts.isequal(R.Properties.VariableNames, {'A', 'C', 'B', 'D'});
R = movevars(T, 'C', 'After', 'C');
asserts.isequal(R.Properties.VariableNames, {'A', 'B', 'C', 'D'});
R = movevars(T, [1 2 3 4], 'After', 'A');
asserts.isequal(R.Properties.VariableNames, {'A', 'B', 'C', 'D'});
%=============================================================================
% A variable listed twice is copied under a unique name.
R = movevars(T, [1 1], 'After', 3);
asserts.isequal(R.Properties.VariableNames, {'B', 'C', 'A', 'A_1', 'D'});
asserts.isequal(R.Properties.VariableUnits, {'ub', 'uc', 'ua', 'ua', 'ud'});
asserts.isequal(R.A_1, [1; 2]);
R = movevars(T, {'B', 'B'}, 'After', 4);
asserts.isequal(R.Properties.VariableNames, {'A', 'C', 'D', 'B', 'B_1'});
%=============================================================================
% No variable to move leaves the table unchanged.
asserts.isequal(movevars(T, [], 'After', 3).Properties.VariableNames, {'A', 'B', 'C', 'D'});
asserts.isequal(movevars(T, {}, 'After', 3).Properties.VariableNames, {'A', 'B', 'C', 'D'});
asserts.isequal(movevars(T, logical([0 0 0 0]), 'Before', 2).Properties.VariableNames, {'A', 'B', 'C', 'D'});
%=============================================================================
% Without a location the variables go to the end.
asserts.isequal(movevars(T, 'A').Properties.VariableNames, {'B', 'C', 'D', 'A'});
asserts.isequal(movevars(T, [1 2]).Properties.VariableNames, {'C', 'D', 'A', 'B'});
%=============================================================================
% Before/After are case-insensitive and may be abbreviated; the last
% occurrence wins.
asserts.isequal(movevars(T, 'A', 'BEFORE', 'C').Properties.VariableNames, {'B', 'A', 'C', 'D'});
asserts.isequal(movevars(T, 'A', 'Bef', 'C').Properties.VariableNames, {'B', 'A', 'C', 'D'});
asserts.isequal(movevars(T, 'A', 'aft', 'C').Properties.VariableNames, {'B', 'C', 'A', 'D'});
asserts.isequal(movevars(T, 'A', "After", 'C').Properties.VariableNames, {'B', 'C', 'A', 'D'});
asserts.isequal(movevars(T, 'A', 'After', 'C', 'After', 'D').Properties.VariableNames, {'B', 'C', 'D', 'A'});
%=============================================================================
% Errors.
msgIndex = 'index must either be real positive integers or logicals.';
msgRange = 'Variable index exceeds table dimensions.';
msgLocation = 'Specify a single location as a positive integer, a variable name, or a logical vector containing a single true value.';
msgNargs = 'Incorrect number of arguments.';
msgBoth = 'Specify either ''Before'' or ''After'', but not both.';
msgSubscript = 'A table variable subscript must be a numeric array containing real positive integers, a logical array, a character vector, a string array, or a cell array of character vectors.';
msgName = 'A table variable name subscript must be a character vector, string array, or cell array of character vectors, with each name in the subscript containing one or more characters.';
assert_checkerror('movevars(T, 5, ''After'', 1)', msgRange, 'Nelson:table:varIndexOutOfRange');
assert_checkerror('movevars(T, 0, ''After'', 1)', msgIndex, 'Nelson:types:invalidIndex:indexEitherRealPositiveIntegersLogicals');
assert_checkerror('movevars(T, 1.5, ''After'', 1)', msgIndex);
assert_checkerror('movevars(T, logical([0 0 0 0 1]), ''After'', 2)', msgRange);
assert_checkerror('movevars(T, 1, ''After'', 5)', msgRange);
assert_checkerror('movevars(T, 1, ''After'', Inf)', msgRange);
assert_checkerror('movevars(T, 1, ''After'', -1)', msgIndex);
assert_checkerror('movevars(T, 1, ''After'', NaN)', msgIndex);
assert_checkerror('movevars(T, 1, ''After'', 1.5)', msgIndex);
assert_checkerror('movevars(T, 3, ''Before'', 0)', msgIndex);
assert_checkerror('movevars(T, 1, ''Before'', logical([0 0 0 0 1]))', msgRange);
assert_checkerror('movevars(T, ''A'', ''After'', logical([0 1 1 0]))', msgLocation, 'Nelson:table:nonscalarPosition');
assert_checkerror('movevars(T, ''A'', ''After'', logical([0 0 0 0]))', msgLocation);
assert_checkerror('movevars(T, ''A'', ''After'', [2 3])', msgLocation);
assert_checkerror('movevars(T, ''A'', ''After'', {''B'', ''C''})', msgLocation);
assert_checkerror('movevars(T, ''A'', ''After'', ["B", "C"])', msgLocation);
assert_checkerror('movevars(T, ''A'', ''After'', [])', msgLocation);
assert_checkerror('movevars(T, ''A'', ''After'', {})', msgLocation);
assert_checkerror('movevars(T, ''Z'', ''After'', 3)', 'Unrecognized table variable name ''Z''.', 'Nelson:table:unrecognizedVariable');
assert_checkerror('movevars(T, ''A'', ''After'', ''Z'')', 'Unrecognized table variable name ''Z''.');
assert_checkerror('movevars(T, 1, ''After'', 3, ''After'', ''Z'')', 'Unrecognized table variable name ''Z''.');
assert_checkerror('movevars(T, {''A'', ''''}, ''After'', 2)', 'Unrecognized table variable name ''''.');
assert_checkerror('movevars(T, '''', ''After'', 3)', msgName, 'Nelson:table:invalidVarName');
assert_checkerror('movevars(T, ''A'', ''After'', '''')', msgName);
assert_checkerror('movevars(T, 1, ''After'', "")', msgName);
assert_checkerror('movevars(T, {1}, ''After'', 2)', msgSubscript, 'Nelson:table:invalidVarSubscript');
assert_checkerror('movevars(T, @isnumeric, ''After'', 4)', msgSubscript);
assert_checkerror('movevars(T, 1, ''After'', {2})', msgSubscript);
assert_checkerror('movevars(T, vartype(''numeric''), ''After'', 4)', 'Using a vartype subscripter to specify variables to move is not supported. Use a character vector, string array, cell array of character vectors, or numeric or logical indices.', 'Nelson:table:vartypeInvalidVars');
assert_checkerror('movevars(T, ''A'', ''After'', vartype(''numeric''))', 'Using a vartype subscripter as a location is not supported. Specify a location as a variable name, a positive integer, or a logical vector containing a single true value.', 'Nelson:table:invalidLocation');
assert_checkerror('movevars(T, ''A'', ''First'')', msgNargs, 'Nelson:table:wrongNumberArgs');
assert_checkerror('movevars(T, ''A'', ''After'')', msgNargs);
assert_checkerror('movevars(T, ''A'', 2)', msgNargs);
assert_checkerror('movevars(T, ''A'', ''After'', ''B'', ''C'')', msgNargs);
assert_checkerror('movevars(T, ''A'', ''After'', ''C'', ''Before'', ''D'')', msgBoth, 'Nelson:table:beforeAndAfter');
assert_checkerror('movevars(T, 1, ''After'', 3, ''Before'', [])', msgBoth);
assert_checkerror('movevars(T, 1, ''before'', ''B'', ''after'', ''D'')', msgBoth);
assert_checkerror('movevars(T, ''A'', ''Foo'', ''C'')', 'Invalid parameter name: Foo.', 'Nelson:table:badParamName');
assert_checkerror('movevars(T, 1, 3, ''C'')', 'Invalid parameter name. Parameter name must be a nonempty string or character vector.', 'Nelson:table:illegalParamName');
assert_checkerror('movevars(T, 1, '''', 3)', 'Invalid parameter name. Parameter name must be a nonempty string or character vector.');
%=============================================================================
% timetable and eventtable keep their row times.
TT = timetable(seconds([1; 2]), [1; 2], [3; 4], [5; 6], 'VariableNames', {'A', 'B', 'C'});
R = movevars(TT, 1, 'After', 3);
asserts.isequal(R.Properties.VariableNames, {'B', 'C', 'A'});
asserts.isequal(R.Properties.RowTimes, seconds([1; 2]));
R = movevars(TT, logical([0 0 1]), 'Before', 'A');
asserts.isequal(R.Properties.VariableNames, {'C', 'A', 'B'});
assert_checkerror('movevars(TT, 4, ''After'', 1)', msgRange);
ET = eventtable(seconds([1; 2]), 'EventLabels', ["a"; "b"]);
ET.X = [1; 2];
R = movevars(ET, 2, 'Before', 1);
asserts.istrue(isa(R, 'eventtable'));
asserts.isequal(R.Properties.VariableNames, {'X', 'EventLabels'});
%=============================================================================
