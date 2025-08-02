%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = planerot(varargin)
  narginchk(1, 1);
  nargoutchk(0, 2);
  X = varargin{1};
  if (size(X, 1) ~= 2) || ~iscolumn(X)
    error(_('First input must be a column vector of length 2.'));
  end
  if X(2) == 0
    G = eye(2, class(X));
  else
    R = norm(X);
    G = [X'; [-X(2), X(1)]] ./ R;
    X = [R; 0];
  end
  
  varargout{1} = G;
  if nargout() > 1
    varargout{2} = X;
  end
end
%=============================================================================
