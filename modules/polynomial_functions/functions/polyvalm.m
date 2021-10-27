%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% Alternatively, you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation; either version 2 of
% the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program. If not, see <http://www.gnu.org/licenses/>.
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
