%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function map = abyss(varargin)
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
  
  firstColor = [0.1490, 0.5490, 0.8667];
  vector = linspace(0.25, 1, m)';
  map = firstColor .* vector;
end
%=============================================================================
