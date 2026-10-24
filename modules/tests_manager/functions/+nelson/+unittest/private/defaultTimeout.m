%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function timeout = defaultTimeout(isBench, runtimeOptions, tagTimeout)
  if nargin < 3
    tagTimeout = 0;
  end
  if ~isempty(runtimeOptions.Timeout)
    timeout = runtimeOptions.Timeout;
    if timeout > 0
      return
    end
  end
  if tagTimeout > 0
    timeout = tagTimeout;
    return
  end
  if isBench
    timeout = 380;
  else
    timeout = 140;
  end
end
