%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function dy = bvpTestVectorizedRhs(x, y)
  global BVP_VECTORIZED_CALL_SEEN;
  if numel(x) > 1
    BVP_VECTORIZED_CALL_SEEN = true;
    dy = [y(2, :); zeros(1, length(x))];
  else
    dy = [y(2); 0];
  end
end
%=============================================================================
