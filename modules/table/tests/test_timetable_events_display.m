%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A timetable with an event table shows the event labels in an unlabelled
% first column.
TT = timetable(seconds([1; 2; 3; 4; 5]), [10; 20; 30; 40; 50], 'VariableNames', {'A'});
TT.Properties.Events = eventtable(seconds([2; 2; 4]), 'EventLabels', ["m"; "p"; "n"]);
R = evalc('TT');
assert_istrue(contains(R, ['5', char(215), '1 timetable with 3 events']));
R = string(splitlines(evalc('disp(TT)')));
REF = ["                  Time     A "; "                  _____    __"; ""; ...
       "                  1 sec    10"; "    <2 events>    2 sec    20"; ...
       "                  3 sec    30"; "    n             4 sec    40"; ...
       "                  5 sec    50"; ""; ""];
assert_isequal(R, REF);
%=============================================================================
% categorical labels, one event
TT.Properties.Events = eventtable(seconds(4), 'EventLabels', categorical("alpha"));
R = evalc('TT');
assert_istrue(contains(R, ['5', char(215), '1 timetable with 1 event']));
R = string(splitlines(evalc('disp(TT)')));
assert_isequal(R(2), "             _____    __");
assert_isequal(R(7), "    alpha    4 sec    40");
assert_isequal(R(6), "             3 sec    30");
%=============================================================================
% numeric and logical labels
TT.Properties.Events = eventtable(seconds([2; 3; 4; 5]), 'EventLabels', [1.23456789; 100000; 0.0001; -0.5]);
R = string(splitlines(evalc('disp(TT)')));
assert_isequal(R(4:8), ["              1 sec    10"; "    1.2346    2 sec    20"; ...
    "    100000    3 sec    30"; "    0.0001    4 sec    40"; "    -0.5      5 sec    50"]);
TT.Properties.Events = eventtable(seconds([2; 4]), 'EventLabels', [true; false]);
R = string(splitlines(evalc('disp(TT)')));
assert_isequal(R([5, 7]), ["    true     2 sec    20"; "    false    4 sec    40"]);
%=============================================================================
% missing or empty labels count as unlabelled events
TT.Properties.Events = eventtable(seconds([2; 4]), 'EventLabels', ["verylonglabel"; missing]);
R = string(splitlines(evalc('disp(TT)')));
assert_isequal(R([5, 7]), ["    verylonglabel    2 sec    20"; "    <1 event>        4 sec    40"]);
TT.Properties.Events = eventtable(seconds([2; 4]), 'EventLabels', [NaN; 3.5]);
R = string(splitlines(evalc('disp(TT)')));
assert_isequal(R([5, 7]), ["    <1 event>    2 sec    20"; "    3.5          4 sec    40"]);
TT.Properties.Events = eventtable(seconds([2; 4]), 'EventLabels', {'ab'; ''});
R = string(splitlines(evalc('disp(TT)')));
assert_isequal(R([5, 7]), ["    ab           2 sec    20"; "    <1 event>    4 sec    40"]);
%=============================================================================
% default labels of an event table without labels
TT.Properties.Events = eventtable(seconds([4; 2; 2]));
R = string(splitlines(evalc('disp(TT)')));
assert_isequal(R([5, 7]), ["    <2 events>    2 sec    20"; "    Event 1       4 sec    40"]);
%=============================================================================
% interval events mark every row in [time, end)
TT.Properties.Events = eventtable(seconds([2; 4]), 'EventLengths', seconds([2; 1]), 'EventLabels', ["x"; "y"]);
R = string(splitlines(evalc('disp(TT)')));
assert_isequal(R(4:8), ["         1 sec    10"; "    x    2 sec    20"; "    x    3 sec    30"; ...
    "    y    4 sec    40"; "         5 sec    50"]);
TT.Properties.Events = eventtable(seconds(1), 'EventEnds', seconds(3), 'EventLabels', "z");
R = string(splitlines(evalc('disp(TT)')));
assert_isequal(R(4:6), ["    z    1 sec    10"; "    z    2 sec    20"; "         3 sec    30"]);
%=============================================================================
% no matching event, or an empty event table: an empty column remains
TT.Properties.Events = eventtable(seconds(10), 'EventLabels', "q");
R = string(splitlines(evalc('disp(TT)')));
assert_isequal(R(1:4), ["        Time     A "; "        _____    __"; ""; "        1 sec    10"]);
TT.Properties.Events = eventtable(seconds(zeros(0, 1)));
R = evalc('TT');
assert_istrue(contains(R, ['5', char(215), '1 timetable']));
assert_isfalse(contains(R, 'event'));
R = string(splitlines(evalc('disp(TT)')));
assert_isequal(R(4), "        1 sec    10");
%=============================================================================
% the row times stay left-aligned under a long dimension name
TT = timetable(seconds([1; 2; 30]), [10; 20; 30], 'VariableNames', {'A'});
TT.Properties.DimensionNames{1} = 'MyLongTimeDimension';
TT.Properties.Events = eventtable(seconds(2), 'EventLabels', "e");
R = string(splitlines(evalc('disp(TT)')));
REF = ["         MyLongTimeDimension    A "; "         ___________________    __"; ""; ...
       "         1 sec                  10"; "    e    2 sec                  20"; ...
       "         30 sec                 30"; ""; ""];
assert_isequal(R, REF);
%=============================================================================
% an empty timetable has no event column
T0 = TT([], :);
R = evalc('T0');
assert_istrue(contains(R, ['0', char(215), '1 empty timetable with 1 event']));
R = string(splitlines(evalc('disp(T0)')));
assert_isequal(R(1), "    MyLongTimeDimension    A");
%=============================================================================
% datetime row times
T2 = timetable(datetime(2020, 1, 1:3)', [1; 2; 3]);
T2.Properties.Events = eventtable(datetime(2020, 1, 2), 'EventLabels', "hello");
R = string(splitlines(evalc('disp(T2)')));
REF = ["                Time        Var1"; "             ___________    ____"; ""; ...
       "             01-Jan-2020     1  "; "    hello    02-Jan-2020     2  "; ...
       "             03-Jan-2020     3  "; ""; ""];
assert_isequal(R, REF);
%=============================================================================
% a 1x1 timetable names its class without size, then its events
T1 = timetable(seconds(1), 5);
T1.Properties.Events = eventtable(seconds(1), 'EventLabels', "e");
R = evalc('T1');
assert_istrue(contains(R, sprintf('\n  timetable with 1 event\n')));
R = string(splitlines(evalc('disp(T1)')));
assert_isequal(R(4), "    e    1 sec     5  ");
T1.Properties.Events = eventtable(seconds([1; 1]));
R = evalc('T1');
assert_istrue(contains(R, sprintf('\n  timetable with 2 events\n')));
%=============================================================================
