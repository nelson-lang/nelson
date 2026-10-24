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
%=============================================================================
% a categorical vector labels the rows, <undefined> for no event
C = categorical(["x"; ""; "y"; "x"]);
E = extractevents(TT, C);
assert_isequal(E.Properties.RowTimes, seconds([1; 3; 4]));
assert_isequal(class(E.EventLabels), 'categorical');
assert_isequal(string(E.EventLabels), ["x"; "y"; "x"]);
assert_isequal(E.Properties.EventLabelsVariable, 'EventLabels');
E = extractevents(TT, C');
assert_isequal(E.Properties.RowTimes, seconds([1; 3; 4]));
E = extractevents(TT, C, 'EventDataVariables', 'A');
assert_isequal(E.Properties.VariableNames, {'A', 'EventLabels'});
E = extractevents(TT, categorical(strings(4, 1)));
assert_isequal(size(E), [0 1]);
msg = _('The number of elements in the labels categorical vector must be the same as the number of rows of the input timetable.');
assert_checkerror('extractevents(TT, categorical("x"));', msg, 'Nelson:eventtable:InvalidLabelsCategorical');
msg = _('Event labels or an event labels variable are not supported when the second input is a categorical.');
assert_checkerror('extractevents(TT, C, ''EventLabels'', "q");', msg, 'Nelson:eventtable:CategoricalAndLabels');
assert_checkerror('extractevents(TT, C, ''EventLabelsVariable'', ''L'');', msg, 'Nelson:eventtable:CategoricalAndLabels');
%=============================================================================
% second output: TT without the event variables, its events kept
[E, T2] = extractevents(TT, [1 2], 'EventLabelsVariable', 'L');
assert_isequal(E.L, ["a"; "b"]);
assert_isequal(T2.Properties.VariableNames, {'A', 'D'});
assert_isequal(T2.Properties.RowTimes, TT.Properties.RowTimes);
assert_isequal(T2.Properties.Events, []);
[E, T2] = extractevents(TT, [1 2], 'EventLengthsVariable', 'D', 'EventDataVariables', 'A');
assert_isequal(T2.Properties.VariableNames, {'L'});
[E, T2] = extractevents(TT, [1 2], 'EventLabelsVariable', 'L', 'PreserveEventVariables', true);
assert_isequal(T2.Properties.VariableNames, {'A', 'L', 'D'});
[E, T2] = extractevents(TT, [1 2]);
assert_isequal(T2.Properties.VariableNames, {'A', 'L', 'D'});
[E, T2] = extractevents(TT, C);
assert_isequal(T2.Properties.VariableNames, {'A', 'L', 'D'});
T3 = TT;
T3.Properties.Events = eventtable(seconds(1));
[E, T2] = extractevents(T3, 2);
assert_isequal(class(T2.Properties.Events), 'eventtable');
assert_isequal(height(T2.Properties.Events), 1);
%=============================================================================
msg = _('In order to specify PreserveEventVariables, provide at least one of EventDataVariables, EventLabelsVariable, EventLengthsVariable, or EventEndsVariable.');
assert_checkerror('extractevents(TT, 1, ''PreserveEventVariables'', false);', msg, 'Nelson:eventtable:ExtracteventsMissingEventVariables');
assert_checkerror('extractevents(TT, 1, ''PreserveEventVariables'', ''x'');', msg, 'Nelson:eventtable:ExtracteventsMissingEventVariables');
msg = _('Using extractevents and specifying PreserveEventVariables without assigning the second output to a workspace variable does not modify the input timetable.');
assert_checkerror('extractevents(TT, 1, ''EventLabelsVariable'', ''L'', ''PreserveEventVariables'', true);', msg, 'Nelson:eventtable:ExtracteventsMissingSecondOutput');
%=============================================================================
