%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
TT = timetable(seconds([1; 2; 3]), [1; 2; 3]);
assert_isequal(class(TT.Properties.Events), 'double');
assert_isequal(size(TT.Properties.Events), [0 0]);
%=============================================================================
R = TT;
R.Properties.Events = seconds([1; 2]);
assert_isequal(class(R.Properties.Events), 'eventtable');
assert_isequal(R.Properties.Events.Properties.RowTimes, seconds([1; 2]));
assert_isequal(R.Properties.Events.EventLabels, ["Event 1"; "Event 2"]);
assert_isequal(class(R(1:2, :).Properties.Events), 'eventtable');
%=============================================================================
R = TT;
R.Properties.Events = seconds([1 2]);
assert_isequal(R.Properties.Events.Properties.RowTimes, seconds([1; 2]));
%=============================================================================
R = TT;
R.Properties.Events = seconds(5);
R.Properties.Events = [];
assert_isequal(R.Properties.Events, []);
%=============================================================================
R = TT;
p = R.Properties;
p.Events = seconds(5);
R.Properties = p;
assert_isequal(R.Properties.Events.EventLabels, "Event 1");
%=============================================================================
R = TT;
R.Properties.Events = timetable(seconds(2), 5);
assert_isequal(class(R.Properties.Events), 'eventtable');
assert_isequal(R.Properties.Events.Var1, 5);
%=============================================================================
TD = timetable(datetime(2020, 1, [1; 2]), [1; 2]);
TD.Properties.Events = datetime(2020, 1, 1);
assert_isequal(class(TD.Properties.Events.Properties.RowTimes), 'datetime');
assert_isequal(TD.Properties.Events.EventLabels, "Event 1");
%=============================================================================
msg = _('The attached event table''s row times must be the same data type as the corresponding timetable''s row times.');
assert_checkerror('R = TT; R.Properties.Events = datetime(2020, 1, [1; 2]);', msg, 'Nelson:timetable:InvalidEventsIncompatibleType');
assert_checkerror('R = TT; R.Properties.Events = eventtable(datetime(2020, 1, 1));', msg, 'Nelson:timetable:InvalidEventsIncompatibleType');
assert_checkerror('R = TD; R.Properties.Events = seconds(1);', msg, 'Nelson:timetable:InvalidEventsIncompatibleType');
%=============================================================================
msg = _('Events property must be an event table.');
assert_checkerror('R = TT; R.Properties.Events = zeros(0, 1);', msg, 'Nelson:timetable:InvalidEvents');
assert_checkerror('R = TT; R.Properties.Events = {};', msg, 'Nelson:timetable:InvalidEvents');
assert_checkerror('R = TT; R.Properties.Events = [1; 2];', msg, 'Nelson:timetable:InvalidEvents');
assert_checkerror('R = TT; R.Properties.Events = table(seconds(2), {''m''});', msg, 'Nelson:timetable:InvalidEvents');
%=============================================================================
R = timetable(seconds([1; 2; 3]), [1; 2; 3], 'Events', seconds(2));
assert_isequal(R.Properties.Events.EventLabels, "Event 1");
R = timetable(seconds([1; 2; 3]), [1; 2; 3], 'Events', []);
assert_isequal(R.Properties.Events, []);
assert_checkerror('timetable(seconds([1; 2; 3]), [1; 2; 3], ''Events'', datetime(2020, 1, 1));', ...
  _('The attached event table''s row times must be the same data type as the corresponding timetable''s row times.'), ...
  'Nelson:timetable:InvalidEventsIncompatibleType');
%=============================================================================
% syncevents does not attach events: it synchronizes the attached ones
msg = _('Timetable must have an event table assigned to the Events property.');
assert_checkerror('syncevents(TT, seconds(2));', msg, 'Nelson:eventtable:NoAttachedEvents');
%=============================================================================
% empty event times: 0x0 removes the events, an empty vector gives a 0x0
% event table
R = TT;
R.Properties.Events = seconds(zeros(0, 1));
assert_isequal(class(R.Properties.Events), 'eventtable');
assert_isequal(size(R.Properties.Events), [0 0]);
R.Properties.Events = seconds(zeros(1, 0));
assert_isequal(class(R.Properties.Events), 'eventtable');
assert_isequal(size(R.Properties.Events), [0 0]);
R.Properties.Events = seconds([]);
assert_isequal(R.Properties.Events, []);
assert_checkerror('eventtable(seconds([]));', 'First input must either be a timetable or it must be a datetime or duration vector.', 'Nelson:eventtable:invalidEventTimes');
%=============================================================================
