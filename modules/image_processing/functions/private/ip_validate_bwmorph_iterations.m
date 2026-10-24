%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function n = ip_validate_bwmorph_iterations(value)
  if ~(isnumeric(value) || islogical(value)) || ~isreal(value) || ~isscalar(value)
    error('Nelson:image_processing:numberIterationsNonnegativeIntegerScalarInf', 'Number of iterations must be a nonnegative integer scalar or Inf.');
  end
  n = double(value);
  if ~(isinf(n) || (isfinite(n) && n >= 0 && n == floor(n)))
    error('Nelson:image_processing:numberIterationsNonnegativeIntegerScalarInf', 'Number of iterations must be a nonnegative integer scalar or Inf.');
  end
end
%=============================================================================
