%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
TT = timetable(seconds([1; 2; 3; 4; 5]), [10; 20; 30; 40; 50], 'VariableNames', {'A'});
%=============================================================================
% events with lengths or ends label the rows in [time, end)
E = timetable(seconds([1.5; 4]), ["m"; "n"], seconds([2; 1]), [5; 6], 'VariableNames', {'L', 'D', 'v'});
TT.Properties.Events = eventtable(E, 'EventLabelsVariable', 'L', 'EventLengthsVariable', 'D');
R = syncevents(TT);
assert_isequal(R.Properties.VariableNames, {'A', 'L', 'v'});
assert_isequal(R.L(2:4), ["m"; "m"; "n"]);
assert_istrue(all(ismissing(R.L([1, 5]))));
assert_isequal(R.v, [NaN; 5; 5; 6; NaN]);
R = syncevents(TT, 'EventDataVariables', ["v", "D"]);
assert_isequal(R.Properties.VariableNames, {'A', 'v', 'D'});
assert_isequal(isnan(R.D), [true; false; false; false; true]);
assert_isequal(R.D(2:4), seconds([2; 2; 1]));
%=============================================================================
TT.Properties.Events = eventtable(seconds([1.5; 4]), 'EventLabels', ["m"; "n"], 'EventEnds', seconds([3; 4]));
R = syncevents(TT);
assert_isequal(R.EventLabels(2), "m");
assert_istrue(all(ismissing(R.EventLabels([1, 3, 4, 5]))));
%=============================================================================
% a row matched by several events is repeated, in event table order
TT.Properties.Events = eventtable(seconds([2; 2; 4]), 'EventLabels', ["p"; "m"; "n"]);
R = syncevents(TT);
assert_isequal(R.Properties.RowTimes, seconds([1; 2; 2; 3; 4; 5]));
assert_isequal(R.A, [10; 20; 20; 30; 40; 50]);
assert_isequal(R.EventLabels([2, 3, 5]), ["p"; "m"; "n"]);
E = timetable(seconds([2; 1]), ["m"; "n"], seconds([2; 3]), 'VariableNames', {'L', 'D'});
TT.Properties.Events = eventtable(E, 'EventLabelsVariable', 'L', 'EventLengthsVariable', 'D');
R = syncevents(TT);
assert_isequal(R.A, [10; 20; 20; 30; 30; 40; 50]);
assert_isequal(R.L(1:5), ["n"; "m"; "n"; "m"; "n"]);
%=============================================================================
% event data variables
E = timetable(seconds([2; 4]), ["m"; "n"], [1; 2], ["a"; "b"], {'c1'; 'c2'}, 'VariableNames', {'L', 'x', 'y', 'z'});
TT.Properties.Events = eventtable(E, 'EventLabelsVariable', 'L');
R = syncevents(TT);
assert_isequal(R.Properties.VariableNames, {'A', 'L', 'x', 'y', 'z'});
assert_isequal(R.x, [NaN; 1; NaN; 2; NaN]);
assert_isequal(R.z, {''; 'c1'; ''; 'c2'; ''});
R = syncevents(TT, 'EventDataVariables', {'y', 'x'});
assert_isequal(R.Properties.VariableNames, {'A', 'y', 'x'});
R = syncevents(TT, 'eventdatavariables', 2);
assert_isequal(R.Properties.VariableNames, {'A', 'x'});
R = syncevents(TT, 'EventDataVariables', [false, true, false, false]);
assert_isequal(R.Properties.VariableNames, {'A', 'x'});
R = syncevents(TT, "q", 'EventDataVariables', "x");
assert_isequal(R.Properties.VariableNames, {'A', 'x'});
R = syncevents(TT, 'EventDataVariables', []);
assert_isequal(R.Properties.VariableNames, {'A'});
assert_checkerror('syncevents(TT, ''EventDataVariables'', "w");', sprintf(_('Unrecognized table variable name ''%s''.'), 'w'));
%=============================================================================
% a variable name already in the timetable: _tt and _et suffixes
R = syncevents(syncevents(TT, 'EventDataVariables', "x"), 'EventDataVariables', "x");
assert_isequal(R.Properties.VariableNames, {'A', 'x_tt', 'x_et'});
assert_isequal(R.x_tt, R.x_et);
%=============================================================================
% types without missing value, matrix variables
E = timetable(seconds(2), int8(3), single(2), datetime(2020, 1, 1), categorical("c"), [1 2], ...
  'VariableNames', {'i', 's', 'd', 'c', 'w'});
T3 = timetable(seconds([1; 2; 3]), [1; 2; 3], 'VariableNames', {'A'});
T3.Properties.Events = eventtable(E);
R = syncevents(T3);
assert_isequal(R.i, int8([0; 3; 0]));
assert_isequal(R.s, single([NaN; 2; NaN]));
assert_isequal(isnat(R.d), [true; false; true]);
assert_isequal(isundefined(R.c), [true; false; true]);
assert_isequal(R.w, [NaN NaN; 1 2; NaN NaN]);
%=============================================================================
% the units and descriptions follow the event variables
E = timetable(seconds([2; 4]), ["m"; "n"], [1; 2], 'VariableNames', {'L', 'v'});
E.Properties.VariableUnits = {'lu', 'vu'};
E.Properties.VariableDescriptions = {'ld', 'vd'};
T3.Properties.Events = eventtable(E, 'EventLabelsVariable', 'L');
R = syncevents(T3);
assert_isequal(R.Properties.VariableUnits, {'', 'lu', 'vu'});
assert_isequal(R.Properties.VariableDescriptions, {'', 'ld', 'vd'});
T3.Properties.Events = eventtable(seconds(2), 'EventLabels', "m");
T3.Properties.VariableUnits = {'u'};
R = syncevents(T3);
assert_isequal(R.Properties.VariableUnits, {'u', ''});
%=============================================================================
