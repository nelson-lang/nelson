%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = triu(varargin)
  A = varargin{1};
  if nargin == 1
    k = 0;
  else
    k = varargin{2};
  end
  [I, J, V, m, n] = IJV(A);
  IJ = [I, J];
  L = find(IJ(:, 1) <= (IJ(:, 2) - k));
  S = IJ(L, :);
  R = sparse(S(:,1), S(:,2), V(L), m, n);
end
%=============================================================================
