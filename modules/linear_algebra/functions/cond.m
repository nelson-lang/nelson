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
function y = cond(A, p)
  narginchk(1, 2);
  if (nargin < 2)
    p = 2;
  end
  
  if ~isequal(p, 2) 
    sizeMismatch = ismatrix(A) && size(A, 1) ~= size(A, 2);
    if sizeMismatch
      error('Nelson:cond:normMismatchSizeA', _('Input must be square.'));
    end
  end
  
  if (isempty(A)) 
    y = 0; 
    return;
  end;
  if (p ~= 2)
    y = norm(A, p) * norm(inv(A), p);
  else
    if (isscalar(A) && (A == 0))
      y = inf;
    else
      s = svd(A);
      if any(s == 0)
        c = cast(Inf, class(A));
      else
        c = max(s) ./ min(s);
        if isempty(c)
          c = zeros(class(A));
        end
      end            
      y = s(1) / s(end);
    end
  end
end
%=============================================================================
