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
function h = invhilb(n, classname)
  % https://nhigham.com/2020/06/30/what-is-the-hilbert-matrix/
  narginchk(1, 2)
  if nargin < 2
    classname = 'double';
  end
  if (isStringScalar(classname)) || (~strcmp(classname,'double') && ~strcmp(classname,'single'))
    error('Nelson:invhilb:notSupportedClass', _('#2 argument must be ''double'' or ''single''.'));
  end
  p = n;
  h = zeros(n, classname);
  for i = 1:n
    r = p * p;
    h(i, i) = r / (2 * i - 1);
    for j = i+1:n
      [h, r] = invh(h, r, n, i, j);
    end
    p = ((n-i) * p * (n+i)) / (i^2);
  end
end
%=============================================================================
function [h, r] = invh(h, r, n, i, j)
  r = -((n - j + 1) * r * (n + j - 1))/(j - 1)^2;
  den = i + j - 1;
  h(i, j) = r / den;
  h(j, i) = r / den;
end
%=============================================================================
