%=============================================================================
% Copyright (c) 2019-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function C = circul(x)
  narginchk(1, 1);
  mustBeNumeric(x, 1);
  n = numel(x);
  if isscalar(x) && x == fix(x) && x > 0
    % scalar positive integer: build vector 1:n
    n = double(x);
    x = 1:n;
  elseif isvector(x) && n > 1
    % valid vector input (n already set)
  else
    error(_('Invalid input argument X.'));
  end
  x = x(:).';
  C = toeplitz ([x(1) x(n:-1:2)], x);
end
%=============================================================================
