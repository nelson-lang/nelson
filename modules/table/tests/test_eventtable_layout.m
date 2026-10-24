%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% the event times are row times, not a variable
ET = eventtable(seconds([1; 2]));
assert_isequal(size(ET), [2 1]);
assert_isequal(height(ET), 2);
assert_isequal(width(ET), 1);
assert_istrue(isa(ET, 'timetable'));
assert_istrue(istimetable(ET));
assert_istrue(istabular(ET));
assert_isfalse(istable(ET));
assert_isequal(superclasses(ET), {'timetable'; 'tabular'});
assert_isequal(ET.Properties.VariableNames, {'EventLabels'});
assert_isequal(ET.Properties.DimensionNames, {'Time', 'Variables'});
assert_isequal(ET.Properties.RowTimes, seconds([1; 2]));
assert_isequal(ET.Time, seconds([1; 2]));
assert_isequal(ET.EventLabels, ["Event 1"; "Event 2"]);
assert_isequal(properties(ET), {'EventLabels'; 'Properties'; 'Time'; 'Variables'});
%=============================================================================
% the event variables head the Properties, which have no Events
P = ET.Properties;
names = fieldnames(P);
assert_isequal(names(1:3), {'EventLabelsVariable'; 'EventLengthsVariable'; 'EventEndsVariable'});
assert_isfalse(isfield(P, 'Events'));
assert_isfalse(isfield(P.CustomProperties, 'EventTimeVariable'));
assert_isequal(P.EventLabelsVariable, 'EventLabels');
assert_isequal(P.EventLengthsVariable, []);
assert_isequal(P.EventEndsVariable, []);
%=============================================================================
% no event gives an empty eventtable without variables
ET = eventtable(seconds(zeros(0, 1)));
assert_isequal(size(ET), [0 0]);
assert_istrue(isempty(ET));
assert_isequal(ET.Properties.EventLabelsVariable, []);
ET = eventtable(seconds(zeros(0, 1)), 'EventLabels', strings(0, 1));
assert_isequal(size(ET), [0 1]);
assert_isequal(ET.Properties.EventLabelsVariable, 'EventLabels');
%=============================================================================
% event ends alone get default labels
ET = eventtable(seconds([1; 2]), 'EventEnds', seconds([3; 4]));
assert_isequal(ET.Properties.VariableNames, {'EventLabels', 'EventEnds'});
assert_isequal(ET.EventLabels, ["Event 1"; "Event 2"]);
assert_isequal(ET.Properties.EventEndsVariable, 'EventEnds');
assert_isequal(ET.Properties.EventLengthsVariable, []);
%=============================================================================
% scalar options are expanded, a row of times is accepted
ET = eventtable(seconds([1 2]), 'EventLabels', "same", 'EventLengths', seconds(3));
assert_isequal(size(ET), [2 2]);
assert_isequal(ET.EventLabels, ["same"; "same"]);
assert_isequal(ET.EventLengths, seconds([3; 3]));
%=============================================================================
% a timetable input: no default labels and no guessed event variables
TT = timetable(seconds([1; 2; 3]), [1; 2; 3], {'a'; 'b'; 'c'}, hours([1; 2; 3]), ...
  'VariableNames', {'A', 'L', 'D'});
ET = eventtable(TT);
assert_isequal(size(ET), [3 3]);
assert_isequal(ET.Properties.EventLabelsVariable, []);
assert_isequal(ET.Properties.EventLengthsVariable, []);
ET = eventtable(TT, 'EventLabelsVariable', 2, 'EventEndsVariable', "D");
assert_isequal(ET.Properties.EventLabelsVariable, 'L');
assert_isequal(ET.Properties.EventEndsVariable, 'D');
ET = eventtable(TT, 'EventLengths', hours([4; 5; 6]));
assert_isequal(ET.Properties.VariableNames, {'A', 'L', 'D', 'EventLengths'});
assert_isequal(ET.Properties.EventLabelsVariable, []);
%=============================================================================
% a new event variable does not replace a variable of the same name
TT = timetable(seconds([1; 2]), [1; 2], 'VariableNames', {'EventLabels'});
ET = eventtable(TT, 'EventLabels', ["a"; "b"]);
assert_isequal(ET.Properties.VariableNames, {'EventLabels', 'EventLabels_1'});
assert_isequal(ET.Properties.EventLabelsVariable, 'EventLabels_1');
%=============================================================================
