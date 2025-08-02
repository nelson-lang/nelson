%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = orth(varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);
  
  A = varargin{1};
  [O, s] = svd(A, 'econ');
  d = diag(s);
  
  if ~isempty(d) && isnan(d(1))
    error(_('Input argument must not contain NaN or Inf.'));
  end
  
  if nargin == 2
    tol = varargin{2};
    if ~(isscalar(tol) && isnumeric(tol) && isreal(tol))
      error(_('Input argument must be a real numeric scalar tolerance'));
    end
  else
    tol = max(size(A)) * eps(norm(d,inf));        
  end
  
  r = sum(d > tol);
  O(:, r + 1:end) = [];
  varargout{1} = O;
end
%=============================================================================
