%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function w = blackmanharris(varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);
  m = varargin{1};
  if nargin < 2
    opt = 'symmetric';
  else
    opt = varargin{2};
  end
  signalValidateWindowLength(m);
  if isempty(m)
    w = zeros(0, 1);
    return;
  end
  if strcmp(opt, 'periodic')
    nmax = m;
  elseif strcmp(opt, 'symmetric') || strcmp(opt, 'symmetric')
    nmax = m - 1;
  else
    error('Nelson:signal:invalidWindowType', '%s expected.', '"periodic" or "symmetric"');
  end
  if m == 1
    w = 1;
  else
    n = (0:m - 1)';
    w = 0.35875 - 0.48829 * cos(2 * pi * n / nmax) ...
      + 0.14128 * cos(4 * pi * n / nmax) - 0.01168 * cos(6 * pi * n / nmax);
  end
end
%=============================================================================
