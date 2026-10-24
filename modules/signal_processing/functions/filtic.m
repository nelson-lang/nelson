%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function zi = filtic(varargin)
  narginchk(3, 4);
  nargoutchk(0, 1);
  b = varargin{1};
  a = varargin{2};
  y = varargin{3};
  if nargin == 4
    x = varargin{4};
  else
    x = [];
  end
  order = max(length(a), length(b)) - 1;
  zi = zeros(order, 1);
  b = [b(:).', zeros(1, order + 1 - length(b))];
  a = [a(:).', zeros(1, order + 1 - length(a))];
  y = y(:);
  x = x(:);
  for k = 1:order
    for m = k:order
      if length(y) >= m - k + 1
        zi(k) = zi(k) - a(m + 1) * y(m - k + 1);
      end
      if length(x) >= m - k + 1
        zi(k) = zi(k) + b(m + 1) * x(m - k + 1);
      end
    end
  end
end
%=============================================================================
