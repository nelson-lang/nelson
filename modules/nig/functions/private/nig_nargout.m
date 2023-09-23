%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function n = nig_nargout(NIG_FUNCTION)
  n = 0;
  for k = NIG_FUNCTION.VARIABLES(:)'
    if strcmp(k.MODE, 'output') || strcmp(k.MODE, 'in_out')
      n = n + 1;
    end
  end
end
%=============================================================================
