%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = __mustBeSorted__(A, dim, direction, missingPlacement, comparisonMethod)
  % Sorted check of mustBeSorted for the inputs without a native path:
  % complex, sparse, string, cell array of character vectors, 'abs'
  % comparison and objects (delegated to their issorted method).
  % dim == 0 selects the first non-singleton dimension.
  if ~(isnumeric(A) || islogical(A) || ischar(A) || isstring(A) || iscellstr(A))
    tf = localObjectIsSorted(A, dim, direction);
    return
  end
  sz = size(A);
  if dim == 0
    dim = find(sz ~= 1, 1);
    if isempty(dim)
      dim = 1;
    end
  end
  if isempty(A) || dim > numel(sz) || sz(dim) <= 1
    tf = true;
    return
  end
  isNumericInput = isnumeric(A) || islogical(A) || ischar(A);
  isComplexInput = isNumericInput && ~isreal(A);
  useAbs = isNumericInput && (strcmp(comparisonMethod, 'abs') || (strcmp(comparisonMethod, 'auto') && isComplexInput));
  useKeys = useAbs || isComplexInput;
  if iscell(A)
    A = string(A);
  end
  if issparse(A)
    A = full(A);
  end
  n = sz(dim);
  X = reshape(permute(A, [dim, 1:dim - 1, dim + 1:numel(sz)]), n, []);
  P = X(1:n - 1, :);
  Q = X(2:n, :);
  missingP = ismissing(P);
  missingQ = ismissing(Q);
  [before, same] = localCompare(P, Q, useKeys, useAbs);
  after = ~before & ~same;
  strict = strncmp(direction, 'strict', 6);
  order = strrep(direction, 'strict', '');
  tf = false;
  if ~strcmp(order, 'descend')
    tf = localInOrder(before, same, missingP, missingQ, strict, ~strcmp(missingPlacement, 'first'));
  end
  if ~tf && ~strcmp(order, 'ascend')
    tf = localInOrder(after, same, missingP, missingQ, strict, strcmp(missingPlacement, 'last'));
  end
end
%=============================================================================
function [before, same] = localCompare(P, Q, useKeys, useAbs)
  % before: P comes strictly before Q in ascending order.
  if ~useKeys
    before = P < Q;
    same = P == Q;
    return
  end
  if useAbs
    if ~isfloat(P)
      P = double(P);
      Q = double(Q);
    end
    keyP = abs(P);
    keyQ = abs(Q);
    tieP = angle(P);
    tieQ = angle(Q);
  else
    keyP = real(P);
    keyQ = real(Q);
    tieP = imag(P);
    tieQ = imag(Q);
  end
  sameKey = keyP == keyQ;
  before = keyP < keyQ | (sameKey & tieP < tieQ);
  same = sameKey & tieP == tieQ;
end
%=============================================================================
function ok = localInOrder(before, same, missingP, missingQ, strict, missingLast)
  if strict
    ok = ~any(missingP(:)) && ~any(missingQ(:)) && all(before(:));
  elseif missingLast
    ok = all(missingQ(:) | (~missingP(:) & (before(:) | same(:))));
  else
    ok = all(missingP(:) | (~missingQ(:) & (before(:) | same(:))));
  end
end
%=============================================================================
function tf = localObjectIsSorted(A, dim, direction)
  if dim == 0 && strcmp(direction, 'ascend')
    tf = issorted(A);
  elseif dim == 0
    tf = issorted(A, direction);
  else
    tf = issorted(A, dim, direction);
  end
  tf = logical(tf);
end
%=============================================================================
