%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function events = timetableCheckedEvents(events, rowTimes)
  % Value stored in the Events property of a timetable with these row
  % times: [], or an event table whose row times have the same type. Event
  % times and timetables are converted with eventtable; 0x0 event times
  % remove the events like [].
  isTimes = isa(events, 'datetime') || isa(events, 'duration');
  if (isa(events, 'double') || isTimes) && isequal(size(events), [0 0])
    events = [];
    return
  end
  if isTimes
    eventTimes = events;
  elseif istimetable(events)
    st = struct(events);
    eventTimes = st.Properties.RowTimes;
  else
    error('Nelson:timetable:InvalidEvents', _('Events property must be an event table.'));
  end
  if ~strcmp(class(eventTimes), class(rowTimes))
    error('Nelson:timetable:InvalidEventsIncompatibleType', _('The attached event table''s row times must be the same data type as the corresponding timetable''s row times.'));
  end
  if ~isa(events, 'eventtable')
    events = eventtable(events);
  end
end
%=============================================================================
