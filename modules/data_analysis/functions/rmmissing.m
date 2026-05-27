%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function B = rmmissing(A, varargin)
  narginchk(1, Inf);
  dim = 1;
  if ~isempty(varargin)
    dim = varargin{1};
  end
  mask = ismissing(A);
  if dim == 1
    B = A(~any(mask, 2), :);
  else
    B = A(:, ~any(mask, 1));
  end
end
%=============================================================================
