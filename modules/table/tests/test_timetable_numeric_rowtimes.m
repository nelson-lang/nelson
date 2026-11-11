%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% numeric row times come from RowTimes, or from a numeric StartTime / TimeStep
Temp = [37.3; 39.1; 42.3];
Pressure = [29.4; 29.6; 30];
TT = timetable(Temp, Pressure, 'RowTimes', [1; 3; 236]);
asserts.isequal(class(TT.Properties.RowTimes), 'double');
asserts.isequal(TT.Properties.RowTimes, [1; 3; 236]);
asserts.isequal(TT.Properties.StartTime, 1);
TT2 = timetable([1; 2; 3], 'SampleRate', 100, 'StartTime', 1);
asserts.isapprox(TT2.Properties.RowTimes, [1; 1.01; 1.02], 1e-12);
TT3 = timetable([1; 2; 3], 'TimeStep', 0.01);
asserts.isapprox(TT3.Properties.RowTimes, [0; 0.01; 0.02], 1e-12);
asserts.isequal(class(timetable([1; 2; 3], 'SampleRate', 100).Properties.RowTimes), 'duration');
asserts.isequal(class(timetable(Temp, 'RowTimes', single([1; 2; 3])).Properties.RowTimes), 'single');
%=============================================================================
% labels select by row time value, a number stays a row position
a = TT(labels(3), :);
asserts.isequal(a.Temp, 39.1);
b = TT(3, :);
asserts.isequal(b.Temp, 42.3);
c = TT(labels([236 1]), :);
asserts.isequal(c.Properties.RowTimes, [236; 1]);
asserts.isequal(height(TT(labels(7), :)), 0);
TT(labels(500), :) = {50, 31};
asserts.isequal(TT.Properties.RowTimes, [1; 3; 236; 500]);
asserts.isequal(TT.Temp(end), 50);
%=============================================================================
s = sortrows(TT, 'Temp', 'descend');
asserts.isequal(s.Properties.RowTimes, [500; 236; 3; 1]);
v = [TT; timetable(1, 2, 'RowTimes', 1000, 'VariableNames', {'Temp', 'Pressure'})];
asserts.isequal(v.Properties.RowTimes(end), 1000);
asserts.istrue(isregular(TT2));
dt = datetime(2024, 1, 1:2)';
TD = timetable(dt, [1; 2]);
asserts.isequal(TD(labels(datetime(2024, 1, 2)), :).Var1, 2);
%=============================================================================
asserts.checkerror('TT(''a'', :)', 'Text cannot select rows of a timetable with numeric row times: use labels.', ...
  'Nelson:timetable:textSubscriptNumericRowTimes');
asserts.checkerror('TD(labels(3), :)', 'Numeric labels select rows of a timetable with numeric row times only.', ...
  'Nelson:timetable:labelsTypeMismatch');
asserts.checkerror('labels({1})', 'Labels must be datetime, duration, double or single values.', ...
  'Nelson:labels:invalidLabels');
asserts.checkerror('timetable([1; 2; 3], Temp)', 'Timetable row times must be datetime or duration values.', ...
  'Nelson:timetable:invalidRowTimes');
%=============================================================================
