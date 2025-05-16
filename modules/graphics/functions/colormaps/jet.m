%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function map = jet(varargin)
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
  n = ceil(m / 4);
  u = [(1:1:n) / n, ones(1, n-1), (n:-1:1) / n]';
  G = ceil(n / 2) - double(mod(m, 4) == 1) + (1:length(u))';
  R = G + n;
  B = G - n;
  R(R > m) = [];
  G(G > m) = [];
  B(B < 1) = [];
  map = zeros(m, 3);
  map(R, 1) = u(1:length(R));
  map(G, 2) = u(1:length(G));
  map(B, 3) = u(end-length(B) + 1:end);
end
%=============================================================================
