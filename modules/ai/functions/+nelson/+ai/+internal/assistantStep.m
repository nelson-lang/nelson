%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Advance the pending claude-agent-acp request of one surface (surfaceId, default
% 'web') by one non-blocking step and return whether it is still running. A host
% that drives the poll itself (the Qt dock, from its own timer) calls this between
% prompts; streamed tokens and the final answer are delivered as assistant
% events, not returned here.
function active = assistantStep(surfaceId)
  narginchk(0, 1);
  nargoutchk(0, 1);
  if nargin < 1
    surfaceId = 'web';
  end
  active = acpAsyncStep(char(surfaceId));
end
%=============================================================================
