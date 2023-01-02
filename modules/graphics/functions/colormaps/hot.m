%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function map = hot(m)
  if nargin < 1
    f = get(groot(), 'CurrentFigure');
    if isempty(f)
      m = 256;
    else
      m = size(f.Colormap,1);
    end
  end
  n = fix(3 /8 * m);
  r = [(1:n)' / n; ones(m - n, 1)];
  g = [zeros(n, 1); (1:n)' / n; ones(m - 2 * n, 1)];
  b = [zeros(2 * n, 1); (1:m-2*n)' / (m-2*n)];
  map = [r g b];
end
%=============================================================================
