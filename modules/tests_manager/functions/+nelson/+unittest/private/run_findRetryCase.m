%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function idx = run_findRetryCase(cases, name)
  idx = 0;
  for k = 1:length(cases)
    if strcmp(cases(k).name, name)
      idx = k;
      return
    end
  end
end
