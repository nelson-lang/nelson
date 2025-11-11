%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = randperm(varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);

  n = varargin{1};
  if(nargin < 2)
    k = n;
  else
    k = varargin{2};
  end
  if (n <= 0)
    varargout{1} = zeros(1, 0);
    return
  end  
  [~, p] = sort(rand(1, n));
  varargout{1} = p(1:k);
end
%=============================================================================
