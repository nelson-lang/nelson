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
  pascalFactor = abs(pascal(nSize, 1, className));
  reverseIndex = nSize:-1:1;
  powerScale = (-2) .^ (0:nSize - 1);
  B = (pascalFactor .* powerScale) * pascalFactor(reverseIndex, reverseIndex);
end
%=============================================================================
