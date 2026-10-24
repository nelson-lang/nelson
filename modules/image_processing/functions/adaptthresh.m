%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function threshold = adaptthresh(varargin)
  narginchk(1, Inf);
  nargoutchk(0, 1);
  img = varargin{1};
  ip_assert_full_image(img, false);
  sensitivity = 0.5;
  argIndex = 2;
  if nargin >= 2 && ~(ischar(varargin{2}) || isstring(varargin{2}))
    sensitivity = double(varargin{2});
    argIndex = 3;
  end
  d = ip_to_unit_double(img);
  nhood = 2 * floor(size(d) / 16) + 1;
  if numel(nhood) < 2
    nhood = [3 3];
  else
    nhood = max(nhood(1:2), [3 3]);
  end
  polarity = 'bright';
  statistic = 'mean';
  while argIndex <= nargin
    if argIndex == nargin
      error('Nelson:image_processing:nameValueArgumentsAppearPairs', 'Name-value arguments must be paired.');
    end
    name = lower(convertStringsToChars(varargin{argIndex}));
    value = varargin{argIndex + 1};
    switch name
      case 'foregroundpolarity'
        polarity = lower(convertStringsToChars(value));
      case 'neighborhoodsize'
        nhood = double(value);
      case 'statistic'
        statistic = lower(convertStringsToChars(value));
      otherwise
        error('Nelson:image_processing:unsupportedAdaptthreshOption', 'Unsupported "%s" option.', 'adaptthresh');
    end
    argIndex = argIndex + 2;
  end
  ip_validate_adaptthresh_options(sensitivity, nhood, polarity, statistic);
  localStat = ip_local_threshold_statistic(d, nhood, statistic);
  if strcmp(polarity, 'bright')
    threshold = localStat * (1 - sensitivity);
  else
    threshold = localStat * (1 + sensitivity);
  end
  threshold = min(max(threshold, 0), 1);
end
%=============================================================================
