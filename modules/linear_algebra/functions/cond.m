%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
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
