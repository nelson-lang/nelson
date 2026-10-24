%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = ifft2(x, r, c)
  narginchk(1, 3);
  nargoutchk(0, 1);
  if nargin() == 2
    error('Nelson:wrongNumberOfInputs', 'Wrong number of input arguments.');
  end
  if nargin() == 1
    y = ifftn(x);
  else
    y = ifftn(x, [r, c]);
  end
end
%=============================================================================
