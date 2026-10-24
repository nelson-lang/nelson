%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function classdefCounterDestroyStateCallback(src, eventData)
  global CLASSDEF_COUNTER_DESTROY_STATE_LOG;
  if isvalid(src)
    state = 'valid';
  else
    state = 'invalid';
  end
  CLASSDEF_COUNTER_DESTROY_STATE_LOG = [eventData.EventName, ':', state, ':', int2str(src.Count)];
end
