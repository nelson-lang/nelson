%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% MinProminence: at least the value, P is not filtered
A = [0 5 1 3 1 4 0];
[TF, P] = islocalmax(A, 'MinProminence', 3);
asserts.isequal(TF, [false true false false false true false]);
asserts.isequal(P, [0 5 0 2 0 3 0]);
asserts.isequal(islocalmax(A, 'MinProminence', 2), [false true false true false true false]);
[TF, P] = islocalmax([0 9 9 1 3 0], 'MinProminence', 5, 'FlatSelection', 'all');
asserts.isequal(TF, [false true true false false false]);
asserts.isequal(P, [0 9 9 0 2 0]);
% the threshold is converted to the class of the prominence
asserts.isequal(islocalmax(uint8([0 2 0 3 0 1 0]), 'MinProminence', 1.5), [false true false true false false false]);
asserts.isequal(islocalmax(uint8([0 2 0 3 0 1 0]), 'MinProminence', 1e6), false(1, 7));
asserts.isequal(islocalmax(logical([0 1 0 1 1 0]), 'MinProminence', 0.2), [false true false true false false]);
asserts.isequal(islocalmax(single([0 0.1 0 0.3 0]), 'MinProminence', 0.1), [false true false true false]);
asserts.isequal(islocalmax([1 5 3 Inf 1 7 1], 'MinProminence', 6), [false false false true false true false]);
%=============================================================================
% MaxNumExtrema: most prominent first, earliest on ties
asserts.isequal(islocalmax(A, 'MaxNumExtrema', 1), [false true false false false false false]);
asserts.isequal(islocalmax([0 5 1 3 1 3 0], 'MaxNumExtrema', 2), [false true false true false false false]);
asserts.isequal(islocalmax([0 9 1 3 0 4 0 3 1 9 0], 'MaxNumExtrema', 3), [false true false false false true false false false true false]);
asserts.isequal(islocalmax([0 9 9 1 3 0], 'MaxNumExtrema', 1, 'FlatSelection', 'all'), [false true true false false false]);
asserts.isequal(islocalmax([1 2 3 Inf NaN Inf 3 2 1], 'MaxNumExtrema', 1), [false false false true false false false false false]);
asserts.isequal(islocalmax([1 5 3 Inf 1 7 1], 'MaxNumExtrema', 2), [false false false true false true false]);
asserts.isequal(islocalmax([1 2 1], 'MaxNumExtrema', Inf), [false true false]);
%=============================================================================
% MinSeparation
asserts.isequal(islocalmax(A, 'MinSeparation', 2), [false true false true false true false]);
asserts.isequal(islocalmax(A, 'MinSeparation', 4), [false true false false false true false]);
asserts.isequal(islocalmax(A, 'MinSeparation', 4, 'MaxNumExtrema', 1), [false true false false false false false]);
asserts.isequal(islocalmax(A, 'MinSeparation', 4, 'MinProminence', 4), [false true false false false false false]);
asserts.isequal(islocalmax([1 3 1 3 1 3 1], 'MinSeparation', 3), [false true false false false true false]);
asserts.isequal(islocalmax([1 3 1 4 1 4 1 3 1], 'MinSeparation', 3), [false false false true false false false true false]);
asserts.isequal(islocalmax([1 3 1 4 1 4 1 3 1], 'MinSeparation', 5), [false false false true false false false false false]);
asserts.isequal(islocalmax([0 9 1 3 0 4 0 3 1 9 0], 'MinSeparation', 4), [false true false false false true false false false true false]);
asserts.isequal(islocalmax([1 3 3 3 1 2 1 3 1], 'MinSeparation', 3), [false false true false false false false true false]);
asserts.isequal(islocalmax([1 3 1 Inf 1 3 1], 'MinSeparation', 3), [false false false true false false false]);
% NaN positions keep their sample point
asserts.isequal(islocalmax([1 NaN 3 NaN 1 NaN 3 NaN 1], 'MinSeparation', 3.9), [false false true false false false true false false]);
asserts.isequal(islocalmax([1 NaN 3 NaN 1 NaN 3 NaN 1], 'MinSeparation', 5), [false false true false false false false false false]);
% with 'last', the extent of an extremum starts one sample before it
A = [NaN; 2; 0; 0; 2; 2; 1; 0; 3; 4; 3; 0; 4];
TF = islocalmax(A, 'FlatSelection', 'last', 'MinProminence', 1, 'MinSeparation', 3.5);
asserts.isequal(find(TF), 10);
TF = islocalmax(A, 'FlatSelection', 'first', 'MinProminence', 1, 'MinSeparation', 3.5);
asserts.isequal(find(TF), [5; 10]);
%=============================================================================
% SamplePoints
A = [0 5 1 3 1 4 0];
x = [1 2 4 8 16 32 64];
asserts.isequal(islocalmax(A, 'SamplePoints', x, 'MinSeparation', 10), [false true false false false true false]);
asserts.isequal(islocalmax([1 5 1 2 1], 'MinSeparation', 0.8, 'SamplePoints', [0 0.5 1 1.3 2]), [false true false true false]);
asserts.isequal(islocalmax([1 2 1], 'SamplePoints', [1 2 3]'), [false true false]);
t = hours(linspace(0, 3, 15));
A = [2 4 6 4 3 7 5 6 5 10 4 -1 -3 -2 0];
asserts.isequal(find(islocalmax(A, 'MinSeparation', minutes(45), 'SamplePoints', t)), [3 10]);
asserts.isequal(find(islocalmax(A, 'MinSeparation', minutes(45), 'SamplePoints', datetime(2020, 1, 1) + t)), [3 10]);
[TF, P] = islocalmax(A, 'ProminenceWindow', minutes(40), 'SamplePoints', t);
asserts.isequal(find(TF), [3 6 8 10]);
asserts.isequal(P, [0 0 2 0 0 2 0 1 0 5 0 0 0 0 0]);
%=============================================================================
% ProminenceWindow: scalar k is [x - k/2, x + k/2), [b f] is [x - b, x + f]
A = [0 5 1 3 1 4 0 1 0 6 0];
[~, P] = islocalmax(A, 'ProminenceWindow', 2);
asserts.isequal(P, [0 5 0 2 0 3 0 1 0 6 0]);
[~, P] = islocalmax(A, 'ProminenceWindow', 3);
asserts.isequal(P, [0 4 0 2 0 3 0 1 0 6 0]);
[~, P] = islocalmax(A, 'ProminenceWindow', [1 2]);
asserts.isequal(P, [0 4 0 2 0 3 0 1 0 6 0]);
[~, P] = islocalmax(A, 'ProminenceWindow', [2 0]);
asserts.isequal(P, [0 5 0 2 0 3 0 1 0 6 0]);
A = [9 0 2 1 5 1 3 0 9];
[~, P] = islocalmax(A, 'ProminenceWindow', 6);
asserts.isequal(P, [0 0 1 0 4 0 2 0 0]);
[~, P] = islocalmax(A, 'ProminenceWindow', 7);
asserts.isequal(P, [0 0 1 0 5 0 2 0 0]);
[~, P] = islocalmax(A, 'ProminenceWindow', [2.5 3.5]);
asserts.isequal(P, [0 0 1 0 4 0 2 0 0]);
% no neighbor inside the window: the prominence is the magnitude of the value
[TF, P] = islocalmax(A, 'ProminenceWindow', 1);
asserts.isequal(TF, [false false true false true false true false false]);
asserts.isequal(P, [0 0 2 0 5 0 3 0 0]);
[~, P] = islocalmax([-10 -5 -9 -7 -9 -6 -10], 'SamplePoints', [1 2 4 8 16 32 64], 'ProminenceWindow', 3);
asserts.isequal(P, [0 5 0 7 0 6 0]);
[~, P] = islocalmax(uint8([3 4 1]), 'ProminenceWindow', 1);
asserts.isequal(P, uint8([0 124 0]));
% a flat region extends the window on both sides
A = [9 0 2 1 5 5 5 1 3 0 9];
[TF, P] = islocalmax(A, 'ProminenceWindow', 5);
asserts.isequal(TF, [false false true false false true false false true false false]);
asserts.isequal(P, [0 0 1 0 4 4 4 0 2 0 0]);
[~, P] = islocalmax(A, 'ProminenceWindow', [3 3]);
asserts.isequal(P, [0 0 1 0 5 5 5 0 2 0 0]);
[~, P] = islocalmax([9 0 2 1 NaN 5 1 3 0 9], 'ProminenceWindow', [3 3]);
asserts.isequal(P, [0 0 1 0 0 4 0 2 0 0]);
%=============================================================================
% tables and timetables
T = table([1; 3; 1; 2; 1], [5; 1; 5; 0; 5], [0; 1; 0; 1; 0], 'VariableNames', {'a', 'b', 'c'});
[TF, P] = islocalmax(T);
asserts.isequal(TF, logical([0 0 0; 1 0 1; 0 1 0; 1 0 1; 0 0 0]));
asserts.istrue(istable(P));
asserts.isequal(P.Properties.VariableNames, {'a', 'b', 'c'});
asserts.isequal(P.a, [0; 2; 0; 1; 0]);
asserts.isequal(P.b, [0; 0; 4; 0; 0]);
[TF, P] = islocalmax(T, 'DataVariables', {'b', 'c'});
asserts.isequal(TF, logical([0 0 0; 0 0 1; 0 1 0; 0 0 1; 0 0 0]));
asserts.isequal(P.Properties.VariableNames, {'b', 'c'});
TF = islocalmax(T, 'DataVariables', [1 3]);
asserts.isequal(TF, logical([0 0 0; 1 0 1; 0 0 0; 1 0 1; 0 0 0]));
[TF, P] = islocalmax(T, 'OutputFormat', 'tabular', 'DataVariables', 'b');
asserts.istrue(istable(TF));
asserts.isequal(TF.Properties.VariableNames, {'b'});
asserts.isequal(TF.b, [false; false; true; false; false]);
asserts.isequal(P.b, [0; 0; 4; 0; 0]);
T2 = table([1; 3; 1; 2; 1], [0; 1; 2; 3; 4], 'VariableNames', {'a', 'x'});
[TF, P] = islocalmax(T2, 'SamplePoints', 'x', 'MinSeparation', 2);
asserts.isequal(TF, logical([0 0; 1 0; 0 0; 1 0; 0 0]));
asserts.isequal(P.Properties.VariableNames, {'a'});
TT = timetable(hours([0; 1; 2; 3; 4]), [1; 3; 1; 3; 1], [2; 0; 2; 0; 2]);
TF = islocalmax(TT, 'MinSeparation', hours(2.5));
asserts.isequal(TF, logical([0 0; 1 0; 0 1; 0 0; 0 0]));
[TF, P] = islocalmax(table(int8([1; 3; 1]), [1; 2; 1]));
asserts.isequal(TF, logical([0 0; 1 1; 0 0]));
asserts.isequal(P.Var1, uint8([0; 2; 0]));
%=============================================================================
% errors
assert_checkerror('islocalmax(''abcba'')', 'First argument must be a numeric or logical array, a table, or a timetable.');
assert_checkerror('islocalmax(''abcba'')', 'First argument must be a numeric or logical array, a table, or a timetable.', 'Nelson:isLocalExtrema:FirstInputInvalid');
assert_checkerror('islocalmax([1 2 1] + 1i)', 'Complex input arguments are not supported.');
assert_checkerror('islocalmax([1 2 1] + 1i)', 'Complex input arguments are not supported.', 'Nelson:isLocalExtrema:ComplexInputArray');
assert_checkerror('islocalmax([1 2 1], 0)', 'Dimension must be a positive integer.');
assert_checkerror('islocalmax([1 2 1], 1.5)', 'Dimension must be a positive integer.', 'Nelson:isLocalExtrema:DimensionInvalid');
assert_checkerror('islocalmax([1 2 1], ''MinProm'')', 'Incorrect number of input arguments. Parameter names and values must occur in pairs.');
assert_checkerror('islocalmax([1 2 1], ''MinProm'')', 'Incorrect number of input arguments. Parameter names and values must occur in pairs.', 'Nelson:isLocalExtrema:NameValuePairs');
msg = 'Parameter name must be ''MaxNumExtrema'', ''MinSeparation'', ''MinProminence'', ''FlatSelection'', ''SamplePoints'', or ''ProminenceWindow''.';
assert_checkerror('islocalmax([1 2 1], ''Min'', 1)', msg);
assert_checkerror('islocalmax([1 2 1], ''Foo'', 1)', msg, 'Nelson:isLocalExtrema:InvalidNameArray');
assert_checkerror('islocalmax([1 2 1], ''MinProminence'', -1)', '''MinProminence'' value must be a nonnegative numeric scalar.');
assert_checkerror('islocalmax([1 2 1], ''MinProminence'', Inf)', '''MinProminence'' value must be a nonnegative numeric scalar.', 'Nelson:isLocalExtrema:MinProminenceInvalid');
assert_checkerror('islocalmax([1 2 1], ''MinSeparation'', -1)', '''MinSeparation'' value must be a nonnegative, real, finite scalar.');
assert_checkerror('islocalmax([1 2 1], ''MinSeparation'', Inf)', '''MinSeparation'' value must be a nonnegative, real, finite scalar.', 'Nelson:isLocalExtrema:MinSeparationInvalid');
assert_checkerror('islocalmax([1 2 1], ''MaxNumExtrema'', 0)', '''MaxNumExtrema'' value must be a positive integer.');
assert_checkerror('islocalmax([1 2 1], ''MaxNumExtrema'', 1.5)', '''MaxNumExtrema'' value must be a positive integer.', 'Nelson:isLocalExtrema:MaxNumInvalid');
assert_checkerror('islocalmax([1 2 1], ''FlatSelection'', ''middle'')', '''FlatSelection'' must be ''first'', ''center'', ''last'', or ''all''.');
assert_checkerror('islocalmax([1 2 1], ''FlatSelection'', ''middle'')', '''FlatSelection'' must be ''first'', ''center'', ''last'', or ''all''.', 'Nelson:isLocalExtrema:FlatSelectionInvalid');
msg = '''ProminenceWindow'' value must be a positive scalar or 2-element vector of nonnegative scalars.';
assert_checkerror('islocalmax([1 2 1], ''ProminenceWindow'', 0)', msg);
assert_checkerror('islocalmax([1 2 1], ''ProminenceWindow'', [1 2 3])', msg, 'Nelson:isLocalExtrema:ProminenceWindowLengthInvalid');
assert_checkerror('islocalmax([1 2 1], ''SamplePoints'', [3 2 1])', '''SamplePoints'' value must be sorted in ascending order.');
assert_checkerror('islocalmax([1 2 1], ''SamplePoints'', [3 2 1])', '''SamplePoints'' value must be sorted in ascending order.', 'Nelson:isLocalExtrema:SamplePointsSorted');
assert_checkerror('islocalmax([1 2 1], ''SamplePoints'', [1 1 2])', '''SamplePoints'' value contains duplicates.', 'Nelson:isLocalExtrema:SamplePointsDuplicate');
assert_checkerror('islocalmax([1 2 1], ''SamplePoints'', [1 NaN 2])', '''SamplePoints'' value must not contain Inf or NaN.', 'Nelson:isLocalExtrema:SamplePointsNonFinite');
assert_checkerror('islocalmax([1 2 1], ''SamplePoints'', [1 2])', 'The sample points vector length must be 3, the same as the size of the input data in the operating dimension.', 'Nelson:isLocalExtrema:SamplePointsLength');
assert_checkerror('islocalmax([1 2 1], ''SamplePoints'', int8([1 2 3]))', '''SamplePoints'' value must be a vector of type double, single, duration, or datetime.', 'Nelson:isLocalExtrema:SamplePointsInvalidDatatype');
assert_checkerror('islocalmax([1 2 1], ''DataVariables'', 1)', '''DataVariables'' parameter is only supported when the first input argument is a table or timetable.', 'Nelson:isLocalExtrema:DataVariablesArray');
assert_checkerror('islocalmax([1 2 1], ''OutputFormat'', ''logical'')', '''OutputFormat'' parameter is only supported when the first input argument is a table or timetable.', 'Nelson:isLocalExtrema:OutputFormatArray');
assert_checkerror('islocalmax([1 2 1], ''MinSeparation'', minutes(3))', '''MinSeparation'' value must be a numeric scalar if the ''SamplePoints'' values are numeric.', 'Nelson:isLocalExtrema:SeparationCannotBeDuration');
assert_checkerror('islocalmax([1 2 1], ''ProminenceWindow'', minutes(3))', '''ProminenceWindow'' value must be numeric if the ''SamplePoints'' values are numeric.', 'Nelson:isLocalExtrema:ProminenceWindowCannotBeDuration');
assert_checkerror('islocalmax([1 2 1], ''MinSeparation'', 3, ''SamplePoints'', hours([1 2 3]))', '''MinSeparation'' value must be a duration scalar if the ''SamplePoints'' values are datetimes or durations.', 'Nelson:isLocalExtrema:SeparationMustBeDuration');
assert_checkerror('islocalmax([1 2 1], ''ProminenceWindow'', 3, ''SamplePoints'', hours([1 2 3]))', '''ProminenceWindow'' value must be a duration if the ''SamplePoints'' values are datetimes or durations.', 'Nelson:isLocalExtrema:ProminenceWindowMustBeDuration');
assert_checkerror('islocalmax(table([1;2;1]), 1)', 'Dimension is not supported when the first argument is a table or timetable.', 'Nelson:isLocalExtrema:DimensionTable');
assert_checkerror('islocalmax(table({''a'';''b'';''c''}))', 'Table variables must be numeric.', 'Nelson:isLocalExtrema:NonNumericTableVar');
assert_checkerror('islocalmax(table([1 2;3 4;1 2]))', 'Table variables must be column vectors.', 'Nelson:isLocalExtrema:NonVectorTableVariable');
assert_checkerror('islocalmax(timetable(hours([0;1;2]), [1;3;1]), ''MinSeparation'', 2)', '''MinSeparation'' value must be a duration scalar when the first input argument is a timetable.', 'Nelson:isLocalExtrema:SeparationMustBeDurationTimetable');
assert_checkerror('islocalmax(timetable(hours([0;1;2]), [1;3;1]), ''SamplePoints'', 1:3)', '''SamplePoints'' parameter not supported when first argument is a timetable.', 'Nelson:isLocalExtrema:SamplePointsTimeTable');
%=============================================================================
