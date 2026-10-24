%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function kernel = ip_gaussian_kernel1d(filterSize, sigma)
  radius = (filterSize - 1) / 2;
  x = -radius:radius;
  kernel = exp(-(x .^ 2) / (2 * sigma ^ 2));
  kernel = kernel / sum(kernel);
end
%=============================================================================
