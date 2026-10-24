%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function eventState = odeInitialEventState(problem, t, y)
  eventState.value = [];
  eventState.isterminal = [];
  eventState.direction = [];
  eventState.isInitial = [];
  if isempty(problem.options.Events)
    return
  end
  [value, isterminal, direction] = odeCallEvents(problem, t, y);
  eventState.value = value(:);
  eventState.isterminal = isterminal(:);
  eventState.direction = direction(:);
  eventState.isInitial = odeLogicalLike(eventState.value, true);
end
%=============================================================================
