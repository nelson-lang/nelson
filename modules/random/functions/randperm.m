%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = randperm(varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);

  n = varargin{1};
  if (nargin < 2)
    k = n;
  else
    k = varargin{2};
  end
  if (n <= 0)
    varargout{1} = zeros(1, 0);
    return
  end
  if (nargin >= 2 && 4 * k < n)
    % Sparse partial permutation: selection sampling ('Floyd') draws exactly
    % one uniform per selected element and reproduces the reference stream.
    avail = 1:n;
    p = zeros(1, k);
    for i = 1:k
      m = n - i + 1;
      j = floor(rand() * m) + 1;
      p(i) = avail(j);
      avail(j) = avail(m);
    end
    varargout{1} = p;
    return
  end
  % Full permutation (matches expected behavior) and the dense partial case.
  [~, p] = sort(rand(1, n));
  varargout{1} = p(1:k);
end
%=============================================================================
