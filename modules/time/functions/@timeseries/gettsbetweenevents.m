%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = gettsbetweenevents(obj, firstEventName, secondEventName)
  firstEvent = timeseriesFindEvent(obj, firstEventName);
  secondEvent = timeseriesFindEvent(obj, secondEventName);
  lo = min(firstEvent.Time, secondEvent.Time);
  hi = max(firstEvent.Time, secondEvent.Time);
  out = getsampleusingtime(obj, lo, hi);
end
%=============================================================================
