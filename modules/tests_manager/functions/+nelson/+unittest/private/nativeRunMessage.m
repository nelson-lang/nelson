%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function message = nativeRunMessage(result)
  if result.timed_out
    message = 'ABORTED';
  elseif result.exit_code == 0
    message = result.stdout;
  elseif ~isempty(result.stderr)
    message = result.stderr;
  else
    message = result.stdout;
  end
end
%=============================================================================
