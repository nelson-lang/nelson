%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% TT.name(rows): the rows of a variable given by row times, a timerange,
% a withtol or text
TT = timetable(seconds([1; 2; 2.05; 3; 4]), [1; 2; 3; 4; 5], ["a"; "b"; "c"; "d"; "e"], 'VariableNames', {'A', 'S'});
assert_isequal(TT.A(seconds(2)), 2);
assert_isequal(TT.A(seconds([3 2])), [4; 2]);
assert_isequal(size(TT.A(seconds(9))), [0 1]);
assert_isequal(TT.S(seconds(4)), "e");
assert_isequal(TT.A(seconds(2), 1), 2);
assert_isequal(TT.A(seconds(2), :), 2);
assert_isequal(TT.A(timerange(seconds(1), seconds(3))), [1; 2; 3]);
assert_isequal(TT.A(withtol(seconds(2), seconds(0.1))), [2; 3]);
assert_isequal(TT.A(2), 2);
assert_isequal(TT.A(:), [1; 2; 3; 4; 5]);
A = TT.A(seconds(2));
assert_isequal(A, 2);
%=============================================================================
T2 = timetable(seconds([1; 2]), [1 2; 3 4]);
assert_isequal(T2.Var1(seconds(2), :), [3 4]);
%=============================================================================
TD = timetable(datetime(2020, 1, 1) + days(0:3)', (1:4)');
assert_isequal(TD.Var1(datetime(2020, 1, 3)), 3);
assert_isequal(TD.Var1('2020-01-03'), 3);
assert_isequal(TD.Var1("2020-01-03"), 3);
assert_isequal(TD('2020-01-03', :).Var1, 3);
msg = sprintf(_('Timetable row subscript must be a numeric, logical, or %s array, or a timerange subscripter.'), 'datetime');
assert_checkerror('TD.Var1(seconds(1));', msg, 'Nelson:timetable:InvalidRowSubscriptsDatetime');
assert_checkerror('TD(seconds(1), :);', msg, 'Nelson:timetable:InvalidRowSubscriptsDatetime');
msg = sprintf(_('Timetable row subscript must be a numeric, logical, or %s array, or a timerange subscripter.'), 'duration');
assert_checkerror('TT.A(datetime(2020, 1, 1));', msg, 'Nelson:timetable:InvalidRowSubscriptsDuration');
%=============================================================================
% assignment: the first row at that time, or a new row
R = TT;
R.A(seconds(2)) = 7;
assert_isequal(R.A, [1; 7; 3; 4; 5]);
R = TT;
R.A(seconds(9)) = 7;
assert_isequal(R.A, [1; 2; 3; 4; 5; 7]);
assert_isequal(R.Properties.RowTimes, seconds([1; 2; 2.05; 3; 4; 9]));
assert_istrue(ismissing(R.S(6)));
R = TT;
R.A(timerange(seconds(1), seconds(3))) = 0;
assert_isequal(R.A, [0; 0; 0; 4; 5]);
%=============================================================================
% event tables
E = eventtable(seconds([1; 2]), 'EventLabels', ["x"; "y"]);
assert_isequal(E.EventLabels(seconds(2)), "y");
assert_isequal(E.EventLabels(1), "x");
%=============================================================================
