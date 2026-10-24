%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
eventTimes = datetime(2022, 11, [3; 5; 10; 14]);
eventType = {'Hail'; 'Rain'; 'Snow'; 'Rain'};
eventLength = hours([1.2; 36; 18; 20]);
precipitation = [12.7; 114.3; 25.4; 177.8];
E = eventtable(eventTimes, ...
  'EventLabels', eventType, ...
  'EventLengths', eventLength);
asserts.isequal(class(E), 'eventtable');
asserts.istrue(isa(E, 'eventtable'));
asserts.istrue(istimetable(E));
asserts.isfalse(istable(E));
asserts.isequal(E.Properties.VariableNames, {'EventLabels', 'EventLengths'});
asserts.isequal(E.Properties.RowTimes, eventTimes);
asserts.isequal(E.Time, eventTimes);
asserts.isequal(E.EventLabels, eventType);
asserts.isequal(E.EventLengths, eventLength);
asserts.isequal(E.Properties.EventLabelsVariable, 'EventLabels');
asserts.isequal(E.Properties.EventLengthsVariable, 'EventLengths');
asserts.isequal(E.Properties.EventEndsVariable, []);
%=============================================================================
eventTT = timetable(eventTimes, eventType, eventLength, precipitation, ...
  'VariableNames', {'EventType', 'EventLength', 'Precipitation'});
E = eventtable(eventTT, ...
  'EventLabelsVariable', 'EventType', ...
  'EventLengthsVariable', 'EventLength');
asserts.isequal(E.Properties.VariableNames, {'EventType', 'EventLength', 'Precipitation'});
asserts.isequal(E.EventType, eventType);
asserts.isequal(E.EventLength, eventLength);
asserts.isequal(E.Precipitation, precipitation);
asserts.isequal(E.Properties.EventLabelsVariable, 'EventType');
asserts.isequal(E.Properties.EventLengthsVariable, 'EventLength');
%=============================================================================
TT = timetable(datetime(2022, 11, (1:3)'), [10; 20; 30], 'VariableNames', {'A'});
TT.Properties.Events = E(1:2, :);
asserts.isequal(class(E(1:2, :)), 'eventtable');
asserts.isequal(TT.Properties.Events, E(1:2, :));
%=============================================================================
E = eventtable(seconds([1; 2]), 'EventLabels', {'start'; 'stop'});
asserts.isequal(E.Properties.EventLabelsVariable, 'EventLabels');
asserts.isequal(E.Properties.EventLengthsVariable, []);
%=============================================================================
E = eventtable(seconds([1; 2]), ...
  'EventLabels', {'start'; 'stop'}, ...
  'EventEnds', seconds([1.5; 3]));
asserts.isequal(E.EventEnds, seconds([1.5; 3]));
asserts.isequal(E.Properties.EventEndsVariable, 'EventEnds');
%=============================================================================
asserts.checkerror('eventtable([1; 2])', _('First input must either be a timetable or it must be a datetime or duration vector.'));
asserts.checkerror('eventtable(seconds([1; 2]), ''EventLabels'', {''a''; ''b''; ''c''})', _('''EventLabels'' must either be a scalar or a vector with the same number of elements as the height of the first input.'));
asserts.checkerror('eventtable(seconds([1; 2]), ''VariableNames'', {''A'', ''B''})', _('Invalid argument name ''VariableNames''. Name must be ''EventLabels'', ''EventLabelsVariable'', ''EventLengths'', ''EventLengthsVariable'', ''EventEnds'', or ''EventEndsVariable''.'));
%=============================================================================
E = eventtable(timetable(seconds([1; 2]), [1; 2], [3; 4]));
asserts.isequal(E.Properties.VariableTypes, ["double", "double"]);
asserts.istrue(isequal(+E, E));
asserts.istrue(isequal(-(-E), E));
asserts.istrue(isequal(E(1:2, :), E));
asserts.isequal(E.Properties.VariableTypes, ["double", "double"]);
asserts.istrue(isequal(+E, E));
%=============================================================================
