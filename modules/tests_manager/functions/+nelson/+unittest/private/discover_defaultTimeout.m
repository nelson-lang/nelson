%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function timeout = discover_defaultTimeout(kind, tagTimeout)
  if nargin < 2
    tagTimeout = 0;
  end
  if tagTimeout > 0
    timeout = tagTimeout;
    return
  end
  if strcmp(kind, 'bench')
    timeout = 380;
  else
    timeout = 140;
  end
end
