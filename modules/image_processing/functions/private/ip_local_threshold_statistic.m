%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function localStat = ip_local_threshold_statistic(d, nhood, statistic)
  if numel(nhood) == 1
    nhood = [nhood nhood];
  end
  switch statistic
    case 'mean'
      localStat = imboxfilt(d, nhood, 'Padding', 'replicate');
    case 'gaussian'
      sigma = max(nhood / 6, [0.5 0.5]);
      localStat = imgaussfilt(d, sigma, 'FilterSize', nhood, 'Padding', 'replicate');
    case 'median'
      localStat = medfilt2(d, nhood, 'replicate');
    otherwise
      error('Nelson:image_processing:unsupportedAdaptiveStatistic', 'Unsupported "%s" option.', 'adaptive statistic');
  end
end
%=============================================================================
