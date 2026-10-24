%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function ip_validate_adaptthresh_options(sensitivity, nhood, polarity, statistic)
  if ~(isscalar(sensitivity) && isfinite(sensitivity) && sensitivity >= 0 && sensitivity <= 1)
    error('Nelson:image_processing:sensitivityFiniteScalarRange', '%s must be a finite scalar in the range [%s].', 'Sensitivity', '0, 1');
  end
  if numel(nhood) == 1
    nhood = [nhood nhood];
  end
  if numel(nhood) ~= 2 || any(~isfinite(nhood(:))) || any(nhood(:) < 1) ...
      || any(nhood(:) ~= floor(nhood(:))) || any(mod(nhood(:), 2) == 0)
    error('Nelson:image_processing:neighborhoodSizeContainPositiveOddIntegers', '%s size must contain positive odd integers.', 'Neighborhood');
  end
  if ~(strcmp(polarity, 'bright') || strcmp(polarity, 'dark'))
    error('Nelson:image_processing:unsupportedForegroundPolarity', 'Unsupported "%s" option.', 'foreground polarity');
  end
  if ~(strcmp(statistic, 'mean') || strcmp(statistic, 'gaussian') || strcmp(statistic, 'median'))
    error('Nelson:image_processing:unsupportedAdaptiveStatistic', 'Unsupported "%s" option.', 'adaptive statistic');
  end
end
%=============================================================================
