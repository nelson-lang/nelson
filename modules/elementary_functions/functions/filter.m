%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = filter(varargin)
  narginchk(3, 3);
  nargoutchk(0, 1);
  b = varargin{1};
  a = varargin{2};
  X = varargin{3};
  
  y = applyNumerator(b, X);
  
  validateDenominatorCoefficient(a);
  varargout{1} = applyDenominator(y, a);
end
%=============================================================================
function y = applyNumerator(b, X)
  y = conv(b, X);
  y = y(1:length(X));
end
%=============================================================================
function validateDenominatorCoefficient(a)
  if a(1) == 0
    error('First denominator filter coefficient must be non-zero.');
  end
end
%=============================================================================
function y = applyDenominator(y, a)
  ap = [0 -1*a(2:end)];
  a1 = a(1);
  
  ya = y;
  N = length(y);
  
  for n = 1:N
    ya = conv(ya, ap);
    ya = ya(1:N);
    y = ya + y;
  end
  
  y = y ./ a1;
end
%=============================================================================
