%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sigma = ip_validate_edge_sigma(sigma, defaultValue)
  if isempty(sigma)
    sigma = defaultValue;
    return
  end
  if ~isnumeric(sigma) || ~isreal(sigma) || ~isscalar(sigma) || ~isfinite(double(sigma)) ...
      || sigma <= 0
    error('Nelson:image_processing:sigmaPositiveFiniteScalar', '%s must be a positive finite scalar.', 'Sigma');
  end
  sigma = double(sigma);
end
%=============================================================================
