%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function W = wilkinson(varargin)
  % https://en.wikipedia.org/wiki/Wilkinson_matrix
  
  narginchk(1, 2);
  nargoutchk(0, 1);
  N = varargin{1};
  mustBeScalarOrEmpty(N, 1);
  mustBeInteger(N, 1);
  mustBeNonnegative(N, 1);
  
  classname = 'double';    
  if nargin == 2
    classname = varargin{2};
  end
  if isStringScalar(classname)
    classname = convertStringsToChars(classname);
  end
  if isempty(N) || (N == 0)
    W = cast([], classname);
    return
  end
  
  N1 = N - 1;
  N12 = cast(N1 / 2, classname);
  S = ones(N1, 1, classname);
  C = abs (-N12 : N12);    
  S = ones (N1, 1);
  W = diag(S, -1) + diag(C) + diag(S, 1);
end
%=============================================================================
