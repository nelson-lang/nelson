%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = isbanded (varargin)
  narginchk(3, 3);
  nargoutchk(0, 1);
  
  A = varargin{1};
  lower = varargin{2};
  if (~isreal (lower) || lower < 0)
    error(_('LOWER must be non-negative integers.'));
  end
  
  upper = varargin{3};
  if (~isreal (upper) || upper < 0)
    error(_('UPPER must be non-negative integers.'));
  end
  
  res = [];
  if ~isempty (A)
    res = (islogical(A) || isnumeric (A)) && (ndims(A) == 2);
    if (res)
      [i, j] = find(A);
      u = j >= i;
      res = all (j(u) - i(u) <= upper);
      if (res)
        l = i >= j;
        res = all(i(l) - j(l) <= lower);
      end
    end
  end
  varargout{1} = res;
end
%=============================================================================

