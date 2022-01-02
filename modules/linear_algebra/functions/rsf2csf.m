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
function varargout = rsf2csf(varargin)
  narginchk(2, 2);
  nargoutchk(0, 2);
  U = varargin{1};
  T = varargin{2};
  n = size(T, 2);
  for m = n:-1:2
    m1 = m - 1;
    if (T(m, m1) ~= 0)
      k = m1:m;
      mu = eig(T(k, k)) - T(m, m);
      r = hypot(mu(1), T(m, m1));
      c = mu(1) / r;
      s = T(m, m1) / r;
      G = [c', s; -s, c];
      T(k, m1:n) = G * T(k, m1:n);
      T(1:m, k) = T(1:m, k) * G';
      U(:, k) = U(:, k) * G';
      T(m, m1) = 0;
    end
  end
  varargout{1} = U;
  if (nargout == 2)
    varargout{2} = T;
  end
end
%=============================================================================
