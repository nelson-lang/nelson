%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = sparsedouble_isequalto(a, b)
  if issparse(b)
    if ~isequalto(size(a), size(b))
      r = false;
    else
      [IA, JA, VA] = IJV(a);
      [IB, JB, VB] = IJV(b);
      r = isequalto(IA, IB) && isequalto(JA, JB) && isequalto(VA, VB);
    end
  else
    r = false;
  end
end
