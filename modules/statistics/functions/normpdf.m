
%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = normpdf(varargin)
  % Normal probability density function
  narginchk(1, 3);
  nargoutchk(0, 1);
  
  x = varargin{1};
  mu = 0;
  sigma = 1;
  
  if nargin > 1
    mu = varargin{2};
  end
  if nargin > 2
    sigma = varargin{3};
  end
  sigma(sigma <= 0) = NaN;
  try
    z = (x - mu) ./ sigma;
    varargout{1} = exp(-0.5 * z .^ 2) ./ (sqrt(2 * pi) .* sigma);
  catch
    error(_('Size mismatch on input arguments'))
  end
end
%=============================================================================
