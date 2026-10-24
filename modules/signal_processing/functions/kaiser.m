%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function w = kaiser(varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);
  m = varargin{1};
  if nargin < 2
    beta = 0.5;
  else
    beta = varargin{2};
  end
  signalValidateWindowLength(m);
  if isempty(m)
    w = zeros(0, 1);
  elseif m == 1
    w = 1;
  else
    n = (0:m - 1)';
    r = 2 * n / (m - 1) - 1;
    w = signalBesseli0(beta * sqrt(1 - r .^ 2)) ./ signalBesseli0(beta);
  end
end
%=============================================================================
