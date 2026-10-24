%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function n = numel(obj, varargin)
  % numel(T) is height(T) * width(T).
  % numel(T, rows, vars) counts the elements T(rows, vars) would select,
  % without checking that the subscripts are in range.
  sz = size(obj);
  if nargin == 1
    n = prod(sz);
    return
  end
  if nargin ~= 3
    error('Nelson:table:invalidSubscript:valueIndexingRequiresRowVariableSubscripts', '%s indexing requires row and variable subscripts.', 'Parentheses');
  end
  n = subscriptCount(obj, varargin, 1) * subscriptCount(obj, varargin, 2);
end
%=============================================================================
function n = subscriptCount(obj, subscripts, dim)
  idx = subscripts{dim};
  if ischar(idx) && strcmp(idx, ':')
    n = size(obj, dim);
  elseif islogical(idx)
    n = nnz(idx);
  elseif ischar(idx)
    n = 1;
  else
    n = numel(idx);
  end
end
%=============================================================================
