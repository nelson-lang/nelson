%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function L = labelmatrix(cc)
  narginchk(1, 1);
  nargoutchk(0, 1);
  L = zeros(cc.ImageSize);
  for k = 1:cc.NumObjects
    L(cc.PixelIdxList{k}) = k;
  end
end
%=============================================================================
