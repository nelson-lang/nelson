%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function ip_validate_edge_threshold(thresh, allowPair)
  if isempty(thresh)
    return
  end
  invalidValues = ~isnumeric(thresh) || ~isreal(thresh) || any(~isfinite(double(thresh(:)))) ...
    || any(thresh(:) < 0);
  if invalidValues && ~allowPair
    error('Nelson:image_processing:thresholdFiniteNumericScalar', '%s must be a finite numeric scalar.', 'Threshold');
  elseif invalidValues
    error('Nelson:image_processing:thresholdContainNonnegativeFiniteValues', 'Threshold must contain nonnegative finite values.');
  end
  if allowPair
    if ~(isscalar(thresh) || numel(thresh) == 2)
      error('Nelson:image_processing:thresholdScalarElementVector', 'Threshold must be a scalar or a 2-element vector.');
    end
    if numel(thresh) == 2 && thresh(1) > thresh(2)
      error('Nelson:image_processing:lowThresholdLessEqualHighThreshold', 'Low threshold must be less than or equal to high threshold.');
    end
  elseif ~isscalar(thresh)
    error('Nelson:image_processing:thresholdFiniteNumericScalar', '%s must be a finite numeric scalar.', 'Threshold');
  end
end
%=============================================================================
