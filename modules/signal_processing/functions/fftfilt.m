%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = fftfilt(varargin)
  narginchk(2, 3);
  nargoutchk(0, 1);
  b = varargin{1};
  x = varargin{2};
  if isvector(x)
    y = filterVector(b, x);
    return;
  end
  y = zeros(size(x));
  for k = 1:size(x, 2)
    y(:, k) = filterVector(b, x(:, k));
  end
end
%=============================================================================
function y = filterVector(b, x)
  y = conv(b(:), x(:));
  y = y(1:length(x));
  if size(x, 1) == 1
    y = y.';
  end
end
%=============================================================================
