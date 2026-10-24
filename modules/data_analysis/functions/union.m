%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [C, ia, ib] = union(A, B, varargin)
  n = max(nargout, 1);
  outputs = cell(1, n);
  [outputs{:}] = setOperation('union', A, B, varargin{:});
  C = outputs{1};
  if n >= 2
    ia = outputs{2};
  end
  if n >= 3
    ib = outputs{3};
  end
end
%=============================================================================
