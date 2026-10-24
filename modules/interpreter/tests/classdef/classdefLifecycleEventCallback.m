%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function classdefLifecycleEventCallback(src, evt)
  % ObjectBeingDestroyed callback: logs "event:<Name>:<isvalid>"
  global CLASSDEF_LIFECYCLE_LOG
  CLASSDEF_LIFECYCLE_LOG{end + 1} = sprintf('event:%s:%d', src.Name, isvalid(src));
end
