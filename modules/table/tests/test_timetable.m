%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
Time = datetime(2024, 1, 1) + days(0:2)';
Temperature = [12.5; 13.0; 11.75];
Pressure = [1012; 1014; 1011];
TT = timetable(Time, Temperature, Pressure);
asserts.istrue(istimetable(TT));
asserts.isfalse(istable(TT));
asserts.istrue(isa(TT, 'tabular'));
asserts.istrue(istabular(TT));
asserts.isequal(height(TT), 3);
asserts.isequal(width(TT), 2);
asserts.isequal(TT.Properties.DimensionNames, {'Time', 'Variables'});
asserts.isequal(TT.Properties.RowTimes, Time);
asserts.isequal(TT.Temperature, Temperature);
asserts.isequal(TT.Pressure, Pressure);
asserts.isequal(TT.Time, Time);
%=============================================================================
TT = timetable(Temperature, Pressure, 'RowTimes', Time, 'VariableNames', {'Temp', 'Press'});
asserts.isequal(TT.Properties.VariableNames, {'Temp', 'Press'});
asserts.isequal(TT{2, 'Temp'}, 13.0);
R = TT(Time(2:3), {'Press'});
asserts.istrue(istimetable(R));
asserts.isequal(R.Properties.RowTimes, Time(2:3));
asserts.isequal(R.Press, [1014; 1011]);
%=============================================================================
TT = timetable([10; 20; 30], 'SampleRate', 2, 'VariableNames', {'Signal'});
asserts.isequal(TT.Properties.RowTimes, seconds([0; 0.5; 1]));
asserts.isequal(TT.Signal, [10; 20; 30]);
%=============================================================================
TT = timetable([10; 20; 30], 'TimeStep', seconds(5), 'StartTime', seconds(10), ...
  'VariableNames', {'Signal'});
asserts.isequal(TT.Properties.RowTimes, seconds([10; 15; 20]));
%=============================================================================
TT = timetable('Size', [2 2], 'VariableTypes', {'double', 'logical'}, ...
  'RowTimes', seconds([1; 2]), 'VariableNames', {'A', 'B'});
asserts.isequal(TT.A, [0; 0]);
asserts.isequal(TT.B, [false; false]);
asserts.isequal(TT.Properties.RowTimes, seconds([1; 2]));
%=============================================================================
A = timetable(seconds([1; 2]), [3; 4], 'VariableNames', {'A'});
B = timetable(seconds([1; 2]), [5; 6], 'VariableNames', {'B'});
C = [A, B];
asserts.isequal(C.A, [3; 4]);
asserts.isequal(C.B, [5; 6]);
D = [A; timetable(seconds(3), 7, 'VariableNames', {'A'})];
asserts.isequal(D.Properties.RowTimes, seconds([1; 2; 3]));
asserts.isequal(D.A, [3; 4; 7]);
%=============================================================================
msg = _('Timetable row times must be datetime or duration values.');
asserts.checkerror('timetable([1; 2], [3; 4])', msg);
msg = _('RowTimes property must contain one time for each row in the timetable.');
asserts.checkerror('timetable([1; 2], ''RowTimes'', seconds(1), ''VariableNames'', {''A''})', msg, 'Nelson:timetable:incorrectNumberOfRowTimes');
%=============================================================================
TT = timetable(seconds([3; 1; 2]), [30; 10; 20], 'VariableNames', {'A'});
[S, I] = sortrows(TT);
asserts.isequal(S.Properties.RowTimes, seconds([1; 2; 3]));
asserts.isequal(S.A, [10; 20; 30]);
asserts.isequal(I, [2; 3; 1]);
asserts.isfalse(issorted(TT));
asserts.istrue(issorted(S));
asserts.istrue(issortedrows(S));
asserts.istrue(isregular(S));
%=============================================================================
L = lag(S);
asserts.isequal(L.Properties.RowTimes, S.Properties.RowTimes);
asserts.istrue(isnan(L.A(1)));
asserts.isequal(L.A(2:3), [10; 20]);
L = lag(S, -1);
asserts.isequal(L.A(1:2), [20; 30]);
asserts.istrue(isnan(L.A(3)));
%=============================================================================
R = retime(S, seconds([1; 1.5; 2; 4]), 'previous');
asserts.isequal(R.A(1:3), [10; 10; 20]);
asserts.isequal(R.A(4), 30);
R = retime(S, seconds([1; 1.5; 2]), 'linear');
asserts.isequal(R.A, [10; 15; 20]);
%=============================================================================
Left = timetable(seconds([1; 3]), [10; 30], 'VariableNames', {'A'});
Right = timetable(seconds([2; 3]), [200; 300], 'VariableNames', {'B'});
U = synchronize(Left, Right);
asserts.isequal(U.Properties.RowTimes, seconds([1; 2; 3]));
asserts.isequal(U.A([1 3]), [10; 30]);
asserts.istrue(isnan(U.A(2)));
asserts.istrue(isnan(U.B(1)));
asserts.isequal(U.B([2 3]), [200; 300]);
I = synchronize(Left, Right, 'intersection');
asserts.isequal(I.Properties.RowTimes, seconds(3));
asserts.isequal(I.A, 30);
asserts.isequal(I.B, 300);
%=============================================================================
T = timetable2table(S);
asserts.istrue(istable(T));
asserts.isequal(T.Time, S.Properties.RowTimes);
asserts.isequal(T.A, S.A);
T = table(seconds([1; 2]), [5; 6], 'VariableNames', {'When', 'Value'});
TT = table2timetable(T);
asserts.isequal(TT.Properties.RowTimes, seconds([1; 2]));
asserts.isequal(TT.Value, [5; 6]);
TT = array2timetable([1 2; 3 4], 'RowTimes', seconds([1; 2]), 'VariableNames', {'A', 'B'});
asserts.isequal(TT.A, [1; 3]);
asserts.isequal(TT.B, [2; 4]);
%=============================================================================
TT = timetable(seconds([1; 2; 3]), [10; 20; 30], 'VariableNames', {'A'});
[tf, rows] = withinrange(TT, seconds([1; 2]));
asserts.isfalse(tf);
asserts.isequal(rows, logical([1; 1; 0]));
[tf, rows] = containsrange(TT, seconds([1.5; 2.5]));
asserts.istrue(tf);
asserts.isequal(rows, logical([0; 1; 0]));
[tf, rows] = overlapsrange(TT, seconds([2.5; 4]));
asserts.istrue(tf);
asserts.isequal(rows, logical([0; 0; 1]));
%=============================================================================
[Top, Idx] = topkrows(TT, 2);
asserts.isequal(Top.A, [30; 20]);
asserts.isequal(Idx, [3; 2]);
Events = eventtable(seconds(2), 'EventLabels', "mark");
TT.Properties.Events = Events;
asserts.isequal(TT.Properties.Events, Events);
R = syncevents(TT);
asserts.isequal(R.EventLabels(R.Properties.RowTimes == seconds(2)), "mark");
asserts.isequal(extractevents(TT, 2).Properties.RowTimes, TT.Properties.RowTimes(2));
%=============================================================================
msg = _('Input must be a timeseries object.');
asserts.checkerror('timeseries2timetable([])', msg);
%=============================================================================
