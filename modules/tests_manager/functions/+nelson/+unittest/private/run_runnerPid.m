%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function pid = run_runnerPid()
  pid = 0;
  try
    if exist('getpid', 'builtin')
      pid = getpid();
    end
  catch
    pid = 0;
  end
end
