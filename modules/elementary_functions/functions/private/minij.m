%=============================================================================
% Copyright (c) 2019-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function A = minij(nSize, className)
  rowVec = 1:cast(nSize, className);
  % Expand vectors explicitly to avoid bsxfun / implicit expansion
  A = min(repmat(rowVec, nSize, 1), repmat(rowVec', 1, nSize));
end
%=============================================================================
