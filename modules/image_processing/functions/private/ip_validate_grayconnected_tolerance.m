%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tolerance = ip_validate_grayconnected_tolerance(tolerance)
  if ~isnumeric(tolerance) || ~isreal(tolerance) || numel(tolerance) ~= 1 ...
      || ~isfinite(tolerance) || tolerance < 0
    error('Nelson:image_processing:toleranceNonnegativeFiniteScalar', '%s must be a nonnegative finite scalar.', 'Tolerance');
  end
  tolerance = double(tolerance);
end
%=============================================================================
