%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% fillmissing: argument validation, time sample points, tables and timetables
%=============================================================================
% MissingLocations: logical array of the size of A; integer and logical data
% only accept the methods that copy or compute values of their own type
ML = [false true false];
msg = 'Fill method must be ''constant'', ''previous'', ''next'', ''nearest'', ''knn'', or a function handle when using ''MissingLocations'' with integer or logical inputs.';
assert_checkerror('fillmissing(int8([1 2 3]), ''linear'', ''MissingLocations'', ML)', msg, 'Nelson:data_analysis:fillmissingIntegerMissingLocations');
assert_checkerror('fillmissing(int8([1 2 3]), ''pchip'', ''MissingLocations'', ML)', msg);
assert_checkerror('fillmissing(int8([1 2 3]), ''movmedian'', 3, ''MissingLocations'', ML)', msg);
assert_checkerror('fillmissing([true false true], ''spline'', ''MissingLocations'', ML)', msg);
assert_isequal(fillmissing(int8([1 2 3]), 'previous', 'MissingLocations', ML), int8([1 1 3]));
assert_isequal(fillmissing(int8([1 2 3]), 'constant', 7, 'MissingLocations', ML), int8([1 7 3]));
assert_isequal(fillmissing(int8([1 2 3]), @(xs, ts, tq) repmat(9, size(tq)), 3, 'MissingLocations', ML), int8([1 9 3]));
assert_isequal(fillmissing(int8([1 2 3]), 'linear'), int8([1 2 3]));
msg = '''MissingLocations'' value must be a logical array of the same size as the first argument.';
assert_checkerror('fillmissing([1 NaN 3], ''linear'', ''MissingLocations'', [true false])', msg, 'Nelson:data_analysis:fillmissingInvalidMissingLocations');
assert_checkerror('fillmissing([1 NaN 3], ''previous'', ''MissingLocations'', [0 1 0])', msg);
%=============================================================================
% fill constant and EndValues: a scalar or one value per slice
A = [1 NaN 3; NaN 5 NaN];
assert_checkerror('fillmissing(A, ''constant'', [10 20])', 'Fill constant must be a scalar or a 3-element vector.', 'Nelson:data_analysis:fillmissingConstantSize');
assert_checkerror('fillmissing(A, ''constant'', [10 20 30 40])', 'Fill constant must be a scalar or a 3-element vector.');
assert_checkerror('fillmissing(A, ''constant'', [10 20 30; 1 2 3])', 'Fill constant must be a scalar or a 3-element vector.');
assert_isequal(fillmissing(A, 'constant', [10; 20; 30]), [1 20 3; 10 5 30]);
assert_checkerror('fillmissing(A, ''linear'', ''EndValues'', [1 2 3 4])', '''EndValues'' value must be a scalar or a 3-element vector.', 'Nelson:data_analysis:fillmissingEndValuesSize');
%=============================================================================
% SamplePoints
assert_checkerror('fillmissing([1 NaN 3], ''linear'', ''SamplePoints'', [1 2])', 'The sample points vector length must be 3, the same as the size of the input data in the operating dimension.', 'Nelson:data_analysis:fillmissingSamplePointsLength');
assert_checkerror('fillmissing([1 NaN 3; 4 5 NaN], ''linear'', ''SamplePoints'', [1 2 3])', 'The sample points vector length must be 2, the same as the size of the input data in the operating dimension.');
assert_isequal(fillmissing([1 NaN 3; 4 5 NaN], 'linear', 2, 'SamplePoints', [1 2 3]), [1 2 3; 4 5 6]);
assert_checkerror('fillmissing([1 NaN 3], ''linear'', ''SamplePoints'', [1 3 2])', '''SamplePoints'' value must be sorted in ascending order.', 'Nelson:data_analysis:fillmissingSamplePointsSorted');
assert_checkerror('fillmissing([1 NaN 3], ''linear'', ''SamplePoints'', [3 2 1])', '''SamplePoints'' value must be sorted in ascending order.');
assert_checkerror('fillmissing([1 NaN 3], ''linear'', ''SamplePoints'', [1 2 2])', '''SamplePoints'' value contains duplicates.', 'Nelson:data_analysis:fillmissingSamplePointsDuplicate');
assert_checkerror('fillmissing([1 NaN 3], ''linear'', ''SamplePoints'', [1 NaN 3])', '''SamplePoints'' value must not contain Inf or NaN.', 'Nelson:data_analysis:fillmissingSamplePointsNonFinite');
assert_checkerror('fillmissing([1 NaN 3], ''linear'', ''SamplePoints'', ''abc'')', '''SamplePoints'' value must be a vector of type double, single, duration, or datetime.', 'Nelson:data_analysis:fillmissingSamplePointsType');
%=============================================================================
% datetime and duration sample points: MaxGap and windows are durations
assert_isapprox(fillmissing([1 NaN 3], 'linear', 'SamplePoints', datetime(2020, 1, [1 2 4])), [1 5 / 3 3], 1e-12);
assert_isequal(fillmissing([1 NaN 3], 'linear', 'SamplePoints', hours([0 1 4])), [1 1.5 3]);
assert_isequal(fillmissing([1 NaN 3 NaN NaN 9], 'linear', 'SamplePoints', hours(0:5), 'MaxGap', hours(2)), [1 2 3 NaN NaN 9]);
assert_isequal(fillmissing([1 NaN 3 NaN 5], 'movmean', days(3), 'SamplePoints', datetime(2020, 1, [1 2 4 7 8])), [1 1 3 5 5]);
msg = '''MaxGap'' data type must be compatible with ''SamplePoints'' data type.';
assert_checkerror('fillmissing([1 NaN 3], ''linear'', ''SamplePoints'', hours([0 1 4]), ''MaxGap'', 2)', msg, 'Nelson:data_analysis:fillmissingMaxGapType');
assert_checkerror('fillmissing([1 NaN 3], ''linear'', ''MaxGap'', hours(2))', msg);
assert_checkerror('fillmissing([1 NaN 3], ''movmean'', 3, ''SamplePoints'', hours([0 1 4]))', 'Invalid data type. Window size must be a duration if ''SamplePoints'' value is a duration or a datetime.', 'Nelson:data_analysis:fillmissingWindowDuration');
assert_checkerror('fillmissing([1 NaN 3], ''movmean'', hours(3))', 'Window size data type must be compatible with ''SamplePoints'' data type.', 'Nelson:data_analysis:fillmissingWindowType');
assert_checkerror('fillmissing([1 NaN 3], @(xs, ts, tq) 0, 3, ''SamplePoints'', hours([0 1 4]))', 'Window length must be a finite positive scalar or 2-element vector of finite nonnegative scalars of type duration.', 'Nelson:data_analysis:fillmissingInvalidGapWindowDuration');
assert_checkerror('fillmissing([1 NaN 3], @(xs, ts, tq) 0, hours(3))', 'Window length must be a finite positive scalar or 2-element vector of finite nonnegative scalars.', 'Nelson:data_analysis:fillmissingInvalidGapWindow');
%=============================================================================
% MaxGap: positive scalar
msg = '''MaxGap'' must be a positive numeric or duration scalar.';
assert_checkerror('fillmissing([1 NaN 3], ''linear'', ''MaxGap'', -1)', msg, 'Nelson:data_analysis:fillmissingInvalidMaxGap');
assert_checkerror('fillmissing([1 NaN 3], ''linear'', ''MaxGap'', [1 2])', msg);
%=============================================================================
% moving windows
msg = 'Window must be a positive scalar or a two-element vector.';
assert_checkerror('fillmissing([1 NaN 3], ''movmean'', 0)', msg, 'Nelson:data_analysis:windowPositiveScalarTwoElement');
assert_checkerror('fillmissing([1 NaN 3], ''movmean'', Inf)', msg);
assert_checkerror('fillmissing([1 NaN 3], ''movmean'', -1)', msg);
assert_checkerror('fillmissing([1 NaN 3], ''movmean'', [1 2 3])', msg);
assert_isequal(fillmissing([1 NaN 3 NaN 5], 'movmean', [0 0]), [1 NaN 3 NaN 5]);
assert_isequal(fillmissing([1 NaN 3 NaN 5], 'movmean', [1 0]), [1 1 3 3 5]);
assert_isequal(fillmissing([1 NaN 3 NaN 5], 'movmean', 2.5), [1 2 3 4 5]);
%=============================================================================
% function handle: gap window, called even without samples, scalar output
msg = 'Window length must be a finite positive scalar or 2-element vector of finite nonnegative scalars.';
f = @(xs, ts, tq) repmat(numel(xs), size(tq));
assert_checkerror('fillmissing([1 NaN 3], f, 0)', msg, 'Nelson:data_analysis:fillmissingInvalidGapWindow');
assert_checkerror('fillmissing([1 NaN 3], f, [0 0])', msg);
assert_checkerror('fillmissing([1 NaN 3], f, -1)', msg);
assert_checkerror('fillmissing([1 NaN 3], f, [NaN 1])', msg);
assert_checkerror('fillmissing([1 NaN 3], f, Inf)', msg);
assert_checkerror('fillmissing([1 NaN 3], f, [1 2 3])', msg);
assert_isequal(fillmissing([1 NaN 3], f, [1 0]), [1 1 3]);
assert_isequal(fillmissing([1 NaN NaN NaN NaN 6], @(xs, ts, tq) repmat(numel(xs) - 1, size(tq)), 1), [1 -1 -1 -1 -1 6]);
assert_isequal(fillmissing([1 NaN NaN 4], @(xs, ts, tq) 7, 3), [1 7 7 4]);
assert_checkerror('fillmissing([1 NaN 3], @(xs, ts, tq) [1 2], 3)', 'Output of function handle must have length 1 or must have the length of the missing value gap.', 'Nelson:data_analysis:fillmissingFunctionOutputSize');
g = @(xs, ts, tq) repmat(hours(tq(1)), size(tq));
assert_isequal(fillmissing([1 NaN 3], g, hours(3), 'SamplePoints', hours([0 1 4])), [1 1 3]);
%=============================================================================
% tables: no dim, one TF column per variable, per-variable values
T = table([1; NaN; 3], [NaN 1; 2 NaN; 3 4], 'VariableNames', {'a', 'b'});
[R, TF] = fillmissing(T, 'previous');
assert_isequal(R.b, [NaN 1; 2 1; 3 4]);
assert_isequal(TF, [false false; true true; false false]);
[R, TF] = fillmissing(T, 'constant', 0, 'ReplaceValues', false);
assert_isequal(R.Properties.VariableNames, {'a', 'b', 'a_filled', 'b_filled'});
assert_isequal(R.b_filled, [0 1; 2 0; 3 4]);
assert_isequal(TF, [false false false true; false false true true; false false false false]);
T2 = table([1; NaN; 3], [NaN; 5; 6], [7; 8; NaN]);
[R, TF] = fillmissing(T2, 'previous', 'DataVariables', [1 3], 'ReplaceValues', false);
assert_isequal(R.Properties.VariableNames, {'Var1', 'Var2', 'Var3', 'Var1_filled', 'Var3_filled'});
assert_isequal(TF, [false false false false false; false false false true false; false false false false true]);
[R, TF] = fillmissing(T2, 'previous', 'DataVariables', [1 3]);
assert_isequal(TF, [false false false; true false false; false false true]);
msg = 'Dimension parameter not supported when first argument is a table or timetable.';
assert_checkerror('fillmissing(T2, ''linear'', 2)', msg, 'Nelson:data_analysis:fillmissingDimensionTable');
assert_checkerror('fillmissing(T2, ''linear'', 1)', msg);
R = fillmissing(T2, 'constant', [10 20 30]);
assert_isequal(R{:, :}, [1 20 7; 10 5 8; 3 6 30]);
R = fillmissing(T2, 'constant', {10, 20, 30});
assert_isequal(R{:, :}, [1 20 7; 10 5 8; 3 6 30]);
R = fillmissing(T2, 'linear', 'EndValues', [10 20 30]);
assert_isequal(R{:, :}, [1 20 7; 2 5 8; 3 6 30]);
R = fillmissing(T2, 'linear', 'SamplePoints', [1 2 4]);
assert_isapprox(R{:, :}, [1 4.5 7; 5 / 3 5 8; 3 6 10], 1e-12);
%=============================================================================
% timetables: the row times are the sample points
TT = timetable(seconds([0; 1; 3]), [1; NaN; 3], [NaN; 5; 6]);
R = fillmissing(TT, 'linear');
assert_istrue(istimetable(R));
assert_isapprox(R{:, :}, [1 4.5; 5 / 3 5; 3 6], 1e-12);
R = fillmissing(TT, 'nearest');
assert_isequal(R{:, :}, [1 5; 1 5; 3 6]);
R = fillmissing(TT, 'linear', 'MaxGap', seconds(2));
assert_isequal(R{:, :}, [1 4.5; NaN 5; 3 6]);
R = fillmissing(TT, 'movmean', seconds(3));
assert_isequal(R{:, :}, [1 5; 1 5; 3 6]);
R = fillmissing(timetable(seconds([0; 1; 3]), [1; NaN; 3]), @(xs, ts, tq) repmat(hours(tq(1)), size(tq)), seconds(2));
assert_isapprox(R{:, :}, [1; 1 / 3600; 3], 1e-15);
assert_checkerror('fillmissing(TT, ''linear'', ''SamplePoints'', [0 1 2])', '''SamplePoints'' parameter not supported when first argument is a timetable.', 'Nelson:data_analysis:fillmissingSamplePointsTimetable');
assert_checkerror('fillmissing(TT, ''linear'', ''MaxGap'', 2)', '''MaxGap'' data type must be compatible with ''SamplePoints'' data type.');
%=============================================================================
