%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
TT = timetable(seconds([1; 2; 3; 4]), [10; 20; 30; 40], ["a"; "b"; "c"; "d"], seconds([1; 2; 3; 4]), ...
  'VariableNames', {'A', 'L', 'D'});
assert_checkerror('extractevents(TT);', sprintf(_('Invalid argument list. Function requires %d more input(s).'), 1), 'Nelson:minrhs');
%=============================================================================
% rows: numbers, logical mask, row times, time range, ':'
E = extractevents(TT, [2 4]);
assert_isequal(class(E), 'eventtable');
assert_isequal(size(E), [2 0]);
assert_isequal(E.Properties.RowTimes, seconds([2; 4]));
E = extractevents(TT, [true false true false]);
assert_isequal(E.Properties.RowTimes, seconds([1; 3]));
E = extractevents(TT, seconds([2; 3]));
assert_isequal(E.Properties.RowTimes, seconds([2; 3]));
E = extractevents(TT, timerange(seconds(2), seconds(4)));
assert_isequal(E.Properties.RowTimes, seconds([2; 3]));
E = extractevents(TT, ':');
assert_isequal(E.Properties.RowTimes, seconds([1; 2; 3; 4]));
E = extractevents(TT, [3 1 3]);
assert_isequal(E.Properties.RowTimes, seconds([3; 1; 3]));
E = extractevents(TT, seconds(2.5));
assert_isequal(size(E), [0 0]);
E = extractevents(TT, []);
assert_isequal(size(E), [0 0]);
E = extractevents(timetable(datetime(2020, 1, 1) + days(0:2)', [1; 2; 3]), 2);
assert_isequal(E.Properties.RowTimes, datetime(2020, 1, 2));
%=============================================================================
% event options: values or variables of TT
E = extractevents(TT, [1 2], 'EventLabels', ["x"; "y"]);
assert_isequal(E.EventLabels, ["x"; "y"]);
assert_isequal(E.Properties.EventLabelsVariable, 'EventLabels');
E = extractevents(TT, [1 2], 'EventLabels', "x");
assert_isequal(E.EventLabels, ["x"; "x"]);
E = extractevents(TT, [1 2], 'EventLabelsVariable', 'L');
assert_isequal(E.Properties.VariableNames, {'L'});
assert_isequal(E.L, ["a"; "b"]);
assert_isequal(E.Properties.EventLabelsVariable, 'L');
E = extractevents(TT, [1 2], 'eventlabelsvariable', 2);
assert_isequal(E.Properties.EventLabelsVariable, 'L');
E = extractevents(TT, [1 2], 'EventLengthsVariable', 'D');
assert_isequal(E.Properties.EventLengthsVariable, 'D');
E = extractevents(TT, [1 2], 'EventEndsVariable', 'D');
assert_isequal(E.Properties.EventEndsVariable, 'D');
E = extractevents(TT, [1 2], 'EventLengths', seconds(1));
assert_isequal(E.EventLengths, seconds([1; 1]));
E = extractevents(TT, [1 2], 'EventEnds', seconds([5; 6]));
assert_isequal(E.EventEnds, seconds([5; 6]));
E = extractevents(TT, [1 2], 'EventDataVariables', 'A');
assert_isequal(E.Properties.VariableNames, {'A'});
assert_isequal(E.A, [10; 20]);
%=============================================================================
% order: event lengths or ends variable, labels variable, data, option values
E = extractevents(TT, [1 2], 'EventLabelsVariable', 'L', 'EventDataVariables', ["A", "D"]);
assert_isequal(E.Properties.VariableNames, {'L', 'A', 'D'});
E = extractevents(TT, [1 2], 'EventDataVariables', "A", 'EventLengthsVariable', 'D', 'EventLabelsVariable', 'L');
assert_isequal(E.Properties.VariableNames, {'D', 'L', 'A'});
E = extractevents(TT, [1 2], 'EventLabels', "x", 'EventDataVariables', ["D", "A"]);
assert_isequal(E.Properties.VariableNames, {'D', 'A', 'EventLabels'});
%=============================================================================
% the metadata of the variables follow them
T2 = TT;
T2.Properties.VariableUnits = {'ua', 'ul', 'ud'};
T2.Properties.Description = 'dd';
E = extractevents(T2, [1 2], 'EventLabelsVariable', 'L', 'EventDataVariables', 'A');
assert_isequal(E.Properties.VariableUnits, {'ul', 'ua'});
assert_isequal(E.Properties.Description, 'dd');
%=============================================================================
msg = _('The second input must be a valid timetable row subscript or a categorical vector.');
assert_checkerror('extractevents(TT, 7);', msg, 'Nelson:eventtable:MustBeCategoricalOrTime');
assert_checkerror('extractevents(TT, 0);', msg, 'Nelson:eventtable:MustBeCategoricalOrTime');
assert_checkerror('extractevents(TT, "a");', msg, 'Nelson:eventtable:MustBeCategoricalOrTime');
assert_checkerror('extractevents(TT, {''a''});', msg, 'Nelson:eventtable:MustBeCategoricalOrTime');
msg = sprintf(_('%s must not be in the list of EventDataVariables.'), 'EventLabelsVariable');
assert_checkerror('extractevents(TT, [1 2], ''EventLabelsVariable'', ''L'', ''EventDataVariables'', ["L", "A"]);', msg, 'Nelson:eventtable:ConflictingEventAndDataVars');
msg = sprintf(_('%s must not be in the list of EventDataVariables.'), 'EventLengthsVariable');
assert_checkerror('extractevents(TT, [1 2], ''EventDataVariables'', ["D", "A"], ''EventLengthsVariable'', ''D'');', msg, 'Nelson:eventtable:ConflictingEventAndDataVars');
assert_checkerror('extractevents(TT, [1 2], ''EventDataVariables'', ''Z'');', sprintf(_('Unrecognized table variable name ''%s''.'), 'Z'));
msg = sprintf(_('Invalid argument at position %d. Make sure name-value arguments include both the name and the value.'), 3);
assert_checkerror('extractevents(TT, [1 2], ''EventLabels'');', msg, 'Nelson:eventtable:nameValuePair');
names = 'EventLabels, EventLabelsVariable, EventLengths, EventLengthsVariable, EventEnds, EventEndsVariable, EventDataVariables, PreserveEventVariables';
msg = sprintf(_('Invalid argument at position %d. Name must be one of these: %s.'), 3, names);
assert_checkerror('extractevents(TT, [1 2], ''Foo'', 1);', msg, 'Nelson:eventtable:invalidArgumentName');
%=============================================================================
