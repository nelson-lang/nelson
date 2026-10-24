%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function timeout = launcherTimeout(testTimeout, runtimeOptions)
  timeout = testTimeout;
  if isempty(testTimeout) || testTimeout <= 0
    return
  end
  timeout = testTimeout + runtimeOptions.LauncherGrace;
end
