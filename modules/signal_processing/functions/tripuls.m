%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = tripuls(varargin)
  narginchk(1, 3);
  nargoutchk(0, 1);
  t = varargin{1};
  width = 1;
  skew = 0;
  if nargin >= 2
    width = varargin{2};
  end
  if nargin == 3
    skew = varargin{3};
  end
  y = zeros(size(t));
  left = -width / 2;
  peak = skew * width / 2;
  right = width / 2;
  if peak == left
    falling = t >= peak & t <= right;
    y(falling) = (right - t(falling)) / (right - peak);
  elseif peak == right
    rising = t >= left & t <= peak;
    y(rising) = (t(rising) - left) / (peak - left);
  else
    rising = t >= left & t <= peak;
    falling = t > peak & t <= right;
    y(rising) = (t(rising) - left) / (peak - left);
    y(falling) = (right - t(falling)) / (right - peak);
  end
end
%=============================================================================
