%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = gettsafteratevent(obj, eventName)
  ev = timeseriesFindEvent(obj, eventName);
  out = timeseriesSubset(obj, find(obj.Time >= ev.Time));
end
%=============================================================================
