%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function map = lines(varargin)
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

  colors = [
    0.0660, 0.4430, 0.7450;
    0.8660, 0.3290, 0.0000;
    0.9290, 0.6940, 0.1250;
    0.5210, 0.0860, 0.8190;
    0.2310, 0.6660, 0.1960;
    0.1840, 0.7450, 0.9370;
    0.8190, 0.0150, 0.5450];

  map = zeros(m, 3);
  for k = 1:m
    map(k, :) = colors(mod(k - 1, size(colors, 1)) + 1, :);
  end
end
%=============================================================================
