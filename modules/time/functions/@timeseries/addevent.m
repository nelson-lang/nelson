%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function obj = addevent(obj, eventValue)
  if ~isa(eventValue, 'tsdata.event')
    error('Nelson:time:inputEventObject', 'Input must be an event object.');
  end
  if isempty(obj.Events)
    obj.Events = eventValue;
  elseif iscell(obj.Events)
    obj.Events{end + 1, 1} = eventValue;
  else
    events = obj.Events;
    events(end + 1, 1) = eventValue;
    obj.Events = events;
  end
end
%=============================================================================
