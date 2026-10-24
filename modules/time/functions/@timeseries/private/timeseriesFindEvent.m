%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function ev = timeseriesFindEvent(obj, eventName)
  eventName = timeseriesText(eventName, 'Event name');
  events = obj.Events;
  for k = 1:numel(events)
    currentEvent = timeseriesEventAt(events, k);
    if strcmp(currentEvent.Name, eventName)
      ev = currentEvent;
      return
    end
  end
  error('Nelson:time:eventFound', 'Event was not found.');
end
%=============================================================================
