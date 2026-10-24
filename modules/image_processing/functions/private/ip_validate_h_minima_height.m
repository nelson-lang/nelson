%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function h = ip_validate_h_minima_height(h)
  if ~(isnumeric(h) || islogical(h)) || ~isreal(h) || ~isscalar(h)
    error('Nelson:image_processing:heightNonnegativeFiniteScalar', '%s must be a nonnegative finite scalar.', 'Height');
  end
  h = double(h);
  if ~(isfinite(h) && h >= 0)
    error('Nelson:image_processing:heightNonnegativeFiniteScalar', '%s must be a nonnegative finite scalar.', 'Height');
  end
end
%=============================================================================
