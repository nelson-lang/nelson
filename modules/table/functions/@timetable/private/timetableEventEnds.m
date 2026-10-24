%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function ends = timetableEventEnds(events)
  % End times of the events of an event table, [] when the events are
  % instantaneous.
  eventVars = eventtable.variables(events);
  ends = [];
  if ~isempty(eventVars.EventLengthsVariable)
    props = subsref(events, struct('type', '.', 'subs', 'Properties'));
    ends = props.RowTimes + subsref(events, struct('type', '.', 'subs', eventVars.EventLengthsVariable));
  elseif ~isempty(eventVars.EventEndsVariable)
    ends = subsref(events, struct('type', '.', 'subs', eventVars.EventEndsVariable));
  end
end
%=============================================================================
