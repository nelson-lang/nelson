%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = acosh(varargin)
  narginchk(1, 1);
  nargoutchk(0, 1);
  
  x = varargin{1};
  if isreal(x)
    if min(x) < 1
      u = acos(x);
      y = 2 * (0.5 - double(imag(u)>0)) .* (u * i);
    else
      y = imag(acos(x));
      y(isnan(x)) = NaN;
    end
  else
    u = acos(x);
    y = 2 * (0.5 - double(imag(u)>0)) .* (u * i);
  end
end
%=============================================================================
