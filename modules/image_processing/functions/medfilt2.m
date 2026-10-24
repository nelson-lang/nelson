%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = medfilt2(varargin)
  narginchk(1, 3);
  nargoutchk(0, 1);
  img = varargin{1};
  ip_assert_full_image(img, false);
  if nargin >= 2
    win = varargin{2};
  else
    win = [3 3];
  end
  padopt = 'zeros';
  if nargin >= 3
    padopt = lower(convertStringsToChars(varargin{3}));
  end
  if numel(win) == 1
    win = [win win];
  end
  win = double(win);
  if numel(win) ~= 2 || any(~isfinite(win(:))) || any(win(:) < 1) ...
      || any(win(:) ~= floor(win(:)))
    error('Nelson:image_processing:neighborhoodSizeContainPositiveIntegers', '%s size must contain positive integers.', 'Neighborhood');
  end
  if ~(strcmp(padopt, 'zeros') || strcmp(padopt, 'indexed') ...
      || strcmp(padopt, 'symmetric') || strcmp(padopt, 'replicate'))
    error('Nelson:image_processing:unsupportedPaddingOption', 'Unsupported "%s" option.', 'padding');
  end
  out = builtin('medfilt2', img, win, padopt);
end
%=============================================================================
