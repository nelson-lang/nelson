%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = asinh(varargin)
  narginchk(1, 1);
  nargoutchk(0, 1);
  
  x = varargin{1};
  if isreal(x)
    y = imag(asin(x * i));
  else
    y = -i * (asin(x * i));
  end
end
%=============================================================================
