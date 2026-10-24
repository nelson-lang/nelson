%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function bw = imbinarize(varargin)
  narginchk(1, Inf);
  nargoutchk(0, 1);
  img = varargin{1};
  method = 'global';
  sensitivity = 0.5;
  polarity = 'bright';
  argIndex = 2;
  if nargin >= 2 && (ischar(varargin{2}) || isstring(varargin{2}))
    method = lower(convertStringsToChars(varargin{2}));
    argIndex = 3;
  elseif nargin >= 2
    level = double(varargin{2});
    argIndex = 3;
  else
    level = graythresh(img);
  end
  while argIndex <= nargin
    if argIndex == nargin
      error('Nelson:image_processing:nameValueArgumentsAppearPairs', 'Name-value arguments must be paired.');
    end
    name = lower(convertStringsToChars(varargin{argIndex}));
    value = varargin{argIndex + 1};
    switch name
      case 'sensitivity'
        sensitivity = double(value);
      case 'foregroundpolarity'
        polarity = lower(convertStringsToChars(value));
      otherwise
        error('Nelson:image_processing:unsupportedImbinarizeOption', 'Unsupported "%s" option.', 'imbinarize');
    end
    argIndex = argIndex + 2;
  end
  d = ip_to_unit_double(img);
  switch method
    case 'global'
      if ~exist('level', 'var')
        level = graythresh(img);
      end
      bw = d > level;
    case 'adaptive'
      threshold = adaptthresh(img, sensitivity, 'ForegroundPolarity', polarity);
      if strcmp(polarity, 'bright')
        bw = d > threshold;
      else
        bw = d < threshold;
      end
    otherwise
      error('Nelson:image_processing:unsupportedImbinarizeMethod', 'Unsupported "%s" option.', 'imbinarize method');
  end
end
%=============================================================================
