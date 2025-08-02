%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = polyvalm (c, x)
  narginchk(2, 2);
  nargoutchk(0, 1);
  
  if ~(isvector (c) || isempty (c))
    error ('Nelson:polyvalm:InvalidP', _('First argument must be a vector.'));
  end
  
  [m, n] = size(x);
  if m ~= n
    error ('Nelson:polyvalm:NonSquareMatrix', _('Second argument must be a square matrix.'));
  end
  
  n = length (c);
  r = size(x, 1);
  if (n == 0)
    y = zeros(r, class (x));
  else
    id = eye(r, class (x));
    y = c(1) * id;
    for i = 2:n
      y = y * x + c(i) * id;
    end
  end
end
%=============================================================================
