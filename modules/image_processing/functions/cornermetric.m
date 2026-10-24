%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function metric = cornermetric(varargin)
  narginchk(1, 7);
  nargoutchk(0, 1);
  img = ip_corner_prepare_image(varargin{1});
  method = 'harris';
  startIndex = 2;
  if nargin >= 2 && (ischar(varargin{2}) || isStringScalar(varargin{2}))
    candidate = lower(convertStringsToChars(varargin{2}));
    if strcmp(candidate, 'harris') || strcmp(candidate, 'minimum eigenvalue') ...
        || strcmp(candidate, 'mineigenvalue')
      method = candidate;
      startIndex = 3;
    elseif mod(nargin - 1, 2) ~= 0
      error('Nelson:image_processing:unsupportedCornerMetricMethod', 'Unsupported "%s" option.', 'corner metric method');
    end
  end
  options = ip_parse_corner_options(varargin(startIndex:nargin));
  metric = ip_corner_metric(img, method, options.FilterSize, options.SensitivityFactor);
end
%=============================================================================
