%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function map = hsv(varargin)
  narginchk(0, 1);
  if nargin < 1
    R = groot();
    f = R.CurrentFigure;
    if isempty(f)
      m = size(R.DefaultFigureColormap, 1);
    else
      m = size(f.Colormap, 1);
    end
  else
    m = varargin{1};
  end

  map = zeros(m, 3);
  if m <= 0
    return
  end

  for k = 1:m
    h = 6 * (k - 1) / m;
    sector = floor(h);
    fraction = h - sector;
    switch sector
      case 0
        map(k, :) = [1, fraction, 0];
      case 1
        map(k, :) = [1 - fraction, 1, 0];
      case 2
        map(k, :) = [0, 1, fraction];
      case 3
        map(k, :) = [0, 1 - fraction, 1];
      case 4
        map(k, :) = [fraction, 0, 1];
      otherwise
        map(k, :) = [1, 0, 1 - fraction];
    end
  end
end
%=============================================================================
