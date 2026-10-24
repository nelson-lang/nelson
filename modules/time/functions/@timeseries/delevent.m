%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function obj = delevent(obj, selector)
  if ischar(selector) || isstring(selector)
    events = obj.Events;
    keep = true(numel(events), 1);
    for k = 1:numel(events)
      currentEvent = timeseriesEventAt(events, k);
      keep(k) = ~strcmp(currentEvent.Name, char(selector));
    end
    obj.Events = events(keep);
  else
    obj.Events(selector) = [];
  end
end
%=============================================================================
