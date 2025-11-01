%=============================================================================
% Copyright (c) 2019-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function B = binomial(nSize, className)
  lowerTri = abs(pascal(nSize, 1, className));
  upperTri = lowerTri(nSize:-1:1, nSize:-1:1);
  diagMat = diag( (-2).^(0:nSize-1) );
  B = lowerTri * diagMat * upperTri;
end
%=============================================================================
