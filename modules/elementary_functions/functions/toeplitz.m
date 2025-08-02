%=============================================================================
% Copyright (c) 2019-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function t = toeplitz(varargin)
  narginchk(1, 2)
  nargoutchk(0, 1)
  
  c = varargin{1};
  if (nargin == 1)
    if ~isvector(c)
      error(_('Input argument must be a vector.'));
    end
    c(1) = conj(c(1));
    r = c;
    c = conj(c);
  else
    r = varargin{2};
  end
  if ~(isnumeric(c) || isnumeric(r))
    error(_('Input arguments must be numeric.'));
  end
  if ~isempty(c) && ~isempty(r) && ~isequaln(r(1),c(1))
    warning(_('Column wins diagonal conflict.'));
  end
  c = c(:);
  m = length(c);    
  r = r(:);
  p = length(r);
  x = [r(p:-1:2, 1) ; c];
  idx = (0 : m-1)' + (p:-1:1);
  t = x(idx);
  if ~isempty(t) && isrow(idx)
    t = t.';
  end
end
%=============================================================================
