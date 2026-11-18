%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Tear down every live assistant session: stop the poll timers and terminate the
% claude-agent-acp agents of all surfaces. The ai module finish hook calls this on
% Nelson exit so no "Thinking..." run and no agent process is left orphaned.
function assistantShutdown()
  narginchk(0, 0);
  nargoutchk(0, 0);
  acpAsyncShutdownAll();
end
%=============================================================================
