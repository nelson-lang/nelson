%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function v = del2(u, varargin)
  narginchk(1, inf);
  if ~(isnumeric(u) || islogical(u))
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Input');
  end
  if isa(u, 'single')
    u = single(u);
  else
    u = double(u);
  end
  nd = ndims(u);
  h = ones(1, nd);
  ns = numel(varargin);
  if ns == 1
    h(:) = spacingScalar(varargin{1});
  elseif ns >= 2
    h(2) = spacingScalar(varargin{1});
    h(1) = spacingScalar(varargin{2});
    for d = 3:min(ns, nd)
      h(d) = spacingScalar(varargin{d});
    end
  end
  acc = zeros(size(u), class(u));
  for d = 1:nd
    acc = acc + secondDiffAlongDim(u, d, h(d));
  end
  v = acc / (2 * nd);
end
%=============================================================================
function D = secondDiffAlongDim(u, d, h)
  sz = size(u);
  n = sz(d);
  D = zeros(sz, class(u));
  if n < 3
    return;
  end
  order = [d, setdiff(1:ndims(u), d)];
  up = permute(u, order);
  szp = size(up);
  m = szp(1);
  rest = prod(szp(2:end));
  up = reshape(up, m, rest);
  interior = (up(1:m - 2, :) - 2 * up(2:m - 1, :) + up(3:m, :)) / (h ^ 2);
  Dp = zeros(m, rest, class(u));
  Dp(2:m - 1, :) = interior;
  if m == 3
    Dp(1, :) = interior(1, :);
    Dp(m, :) = interior(1, :);
  else
    Dp(1, :) = 2 * interior(1, :) - interior(2, :);
    Dp(m, :) = 2 * interior(m - 2, :) - interior(m - 3, :);
  end
  Dp = reshape(Dp, szp);
  D = ipermute(Dp, order);
end
%=============================================================================
function s = spacingScalar(value)
  if ~isnumeric(value) || ~isscalar(value)
    error('Nelson:del2:invalidSpacing', 'Spacing must be a numeric scalar.');
  end
  s = double(value);
end
%=============================================================================
