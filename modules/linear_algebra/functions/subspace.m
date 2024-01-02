%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function t = subspace(varargin)
  narginchk(2, 2);
  nargoutchk(0, 1);
  
  A = orth(varargin{1});
  B = orth(varargin{2});
  if size(A, 2) < size(B, 2)
    C = B;
    B = A;
    A = C;
  end
  B = B - A * (A' * B);
  t = asin(min(1, norm(B)));
end
%=============================================================================
