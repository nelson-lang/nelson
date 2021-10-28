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
function y = pinv(varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);
  A = varargin{1};
  if (isempty(A))
    y = A;
    return;
  end
  [U, S, V] = svd(A, 'econ');
  s = diag(S);
  if nargin < 2 
    tol = max(size(A)) * eps(norm(s, inf));
  else
    tol = varargin{2};
  end
  r1 = sum(s > tol) + 1;
  V(:, r1:end) = [];
  U(:, r1:end) = [];
  s(r1:end) = [];
  s = 1 ./ s(:);
  y = (V .* s.') * U';
end
%=============================================================================
