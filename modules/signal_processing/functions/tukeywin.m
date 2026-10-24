%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function w = tukeywin(varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);
  m = varargin{1};
  if nargin < 2
    r = 0.5;
  else
    r = varargin{2};
  end
  signalValidateWindowLength(m);
  if isempty(m)
    w = zeros(0, 1);
    return;
  end
  if r <= 0
    w = ones(m, 1);
    return;
  end
  if r >= 1
    w = hann(m);
    return;
  end
  n = (0:m - 1)';
  w = ones(m, 1);
  first = n < r * (m - 1) / 2;
  last = n >= (m - 1) * (1 - r / 2);
  w(first) = 0.5 * (1 + cos(pi * (2 * n(first) / (r * (m - 1)) - 1)));
  w(last) = 0.5 * (1 + cos(pi * (2 * n(last) / (r * (m - 1)) - 2 / r + 1)));
end
%=============================================================================
