%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = setOperation(op, A, B, varargin)
  [stable, byRows] = parseSetOptions(varargin{:});
  if byRows
    [varargout{1:max(1, nargout)}] = setOperationRows(op, A, B, stable);
    return
  end
  a = A(:);
  b = B(:);
  if issparse(a)
    a = full(a);
  end
  if issparse(b)
    b = full(b);
  end
  useKeys = ~(isComparableByValueLocal(a) && isComparableByValueLocal(b));
  if useKeys
    keysA = valuesToKeys(a);
    keysB = valuesToKeys(b);
    [uaVals, uaIdx] = uniqueByKeysLocal(a, keysA, stable);
    [ubVals, ubIdx] = uniqueByKeysLocal(b, keysB, stable);
    [tfA, locB] = ismemberKeys(valuesToKeys(uaVals), keysB);
    [tfB, ~] = ismemberKeys(valuesToKeys(ubVals), keysA);
  else
    [uaVals, uaIdx] = uniqueValuesLocal(a, stable);
    [ubVals, ubIdx] = uniqueValuesLocal(b, stable);
    [tfA, locB] = memberValuesLocal(uaVals, b);
    [tfB, ~] = memberValuesLocal(ubVals, a);
  end
  switch op
    case 'intersect'
      C = uaVals(tfA);
      ia = uaIdx(tfA);
      ib = locB(tfA);
      values = {reshapeLikeInputs(C, A, B), ia(:), ib(:)};
    case 'setdiff'
      keep = ~tfA;
      C = uaVals(keep);
      ia = uaIdx(keep);
      % setdiff result orientation only depends on A (aligned with expected behavior)
      values = {reshapeLikeInputs(C, A, A), ia(:)};
    case 'union'
      onlyB = ~tfB;
      cA = uaVals;
      cB = ubVals(onlyB);
      iaAll = uaIdx;
      ibAll = ubIdx(onlyB);
      [C, ia, ib] = mergePartsLocal(cA, cB, iaAll, ibAll, stable, useKeys);
      values = {reshapeLikeInputs(C, A, B), ia(:), ib(:)};
    case 'setxor'
      keepA = ~tfA;
      keepB = ~tfB;
      cA = uaVals(keepA);
      cB = ubVals(keepB);
      iaAll = uaIdx(keepA);
      ibAll = ubIdx(keepB);
      [C, ia, ib] = mergePartsLocal(cA, cB, iaAll, ibAll, stable, useKeys);
      values = {reshapeLikeInputs(C, A, B), ia(:), ib(:)};
    otherwise
      error('Nelson:data_analysis:unsupportedSetOperation', 'Unsupported set operation.');
  end
  varargout = values(1:max(1, nargout));
end
%=============================================================================
function [stable, byRows] = parseSetOptions(varargin)
  stable = false;
  byRows = false;
  for k = 1:length(varargin)
    option = varargin{k};
    if isstring(option)
      if ~isscalar(option)
        error('Nelson:data_analysis:unsupportedOption', 'Unsupported option.');
      end
      option = char(option);
    end
    if ischar(option) && strcmpi(option, 'stable')
      stable = true;
    elseif ischar(option) && strcmpi(option, 'sorted')
      stable = false;
    elseif ischar(option) && strcmpi(option, 'rows')
      byRows = true;
    else
      error('Nelson:data_analysis:unsupportedOption', 'Unsupported option.');
    end
  end
end
%=============================================================================
function varargout = setOperationRows(op, A, B, stable)
  if ~isempty(A) && ~isempty(B) && (size(A, 2) ~= size(B, 2))
    error('Nelson:data_analysis:inputsSameNumberColumns', 'Inputs must have the same number of columns.');
  end
  [uaVals, uaIdx] = uniqueRowsLocal(A, stable);
  [ubVals, ubIdx] = uniqueRowsLocal(B, stable);
  [tfA, locB] = ismemberRowsLocal(uaVals, B);
  [tfB, ~] = ismemberRowsLocal(ubVals, A);
  switch op
    case 'intersect'
      C = uaVals(tfA, :);
      values = {C, uaIdx(tfA), locB(tfA)};
    case 'setdiff'
      keep = ~tfA;
      values = {uaVals(keep, :), uaIdx(keep)};
    case 'union'
      onlyB = ~tfB;
      [C, ia, ib] = mergeRowsLocal(uaVals, ubVals(onlyB, :), uaIdx, ubIdx(onlyB), stable);
      values = {C, ia, ib};
    case 'setxor'
      keepA = ~tfA;
      keepB = ~tfB;
      [C, ia, ib] = mergeRowsLocal(uaVals(keepA, :), ubVals(keepB, :), ...
        uaIdx(keepA), ubIdx(keepB), stable);
      values = {C, ia, ib};
    otherwise
      error('Nelson:data_analysis:unsupportedSetOperation', 'Unsupported set operation.');
  end
  varargout = values(1:max(1, nargout));
end
%=============================================================================
function [U, idx] = uniqueRowsLocal(M, stable)
  % Delegates to the vectorized builtin unique('rows') (first-occurrence row
  % index), far faster than the interpreted row-compare loop.
  if size(M, 1) == 0
    U = M;
    idx = zeros(0, 1);
    return
  end
  if stable
    [U, idx] = unique(M, 'rows', 'stable');
  else
    [U, idx] = unique(M, 'rows');
  end
  idx = idx(:);
end
%=============================================================================
function [tf, loc] = ismemberRowsLocal(Q, S)
  % Delegates to the vectorized builtin ismember('rows') (first match),
  % replacing the O(nq*ns) nested row-compare loop.
  [tf, loc] = ismember(Q, S, 'rows');
  tf = tf(:);
  loc = loc(:);
end
%=============================================================================
function [C, ia, ib] = mergeRowsLocal(cA, cB, iaAll, ibAll, stable)
  nA = size(cA, 1);
  C0 = [cA; cB];
  if stable || isempty(C0)
    C = C0;
    ia = iaAll;
    ib = ibAll;
    return
  end
  [C, p] = sortrows(C0);
  fromA = (p <= nA);
  ia = iaAll(p(fromA));
  ib = ibAll(p(~fromA) - nA);
end
%=============================================================================
function tf = isComparableByValueLocal(x)
  tf = isnumeric(x) || islogical(x) || ischar(x) || isstring(x) || iscellstr(x);
end
%=============================================================================
function [C, ia, ib] = mergePartsLocal(cA, cB, iaAll, ibAll, stable, useKeys)
  % combines the part coming from A and the part coming only from B,
  % keeping track of which output indices refer to A and which to B.
  nA = numel(cA);
  C0 = [cA; cB];
  if stable || isempty(C0)
    C = C0;
    ia = iaAll;
    ib = ibAll;
    return
  end
  if useKeys
    [C, p] = sortByKeysLocal(C0);
  else
    [C, p] = sort(C0);
    C = C0(p);
  end
  fromA = (p <= nA);
  ia = iaAll(p(fromA));
  ib = ibAll(p(~fromA) - nA);
end
%=============================================================================
function [C, ic] = uniqueValuesLocal(x, stable)
  % Delegates to the vectorized builtin unique (first-occurrence indices),
  % far faster than an interpreted element loop; NaN kept, -0 == 0, no loss.
  x = x(:);
  if isempty(x)
    C = x;
    ic = zeros(0, 1);
    return
  end
  if stable
    [C, ic] = unique(x, 'stable');
  else
    [C, ic] = unique(x);
  end
  C = C(:);
  ic = ic(:);
end
%=============================================================================
function [tf, loc] = memberValuesLocal(vals, source)
  % Delegates to the vectorized builtin ismember (first-occurrence index),
  % handling real, complex and NaN inputs far faster than a loop.
  [tf, loc] = ismember(vals(:), source(:));
  tf = tf(:);
  loc = loc(:);
end
%=============================================================================
function [C, ic] = uniqueByKeysLocal(vals, keys, stable)
  % unique for cell/string values compared through their keys.
  % ic contains the index of the first occurrence in vals.
  firstIdx = [];
  seen = {};
  for k = 1:length(keys)
    if ~any(strcmp(seen, keys{k}))
      seen{end + 1} = keys{k};
      firstIdx(end + 1) = k;
    end
  end
  ic = firstIdx(:);
  C = vals(ic);
  if ~stable && ~isempty(ic)
    [C, p] = sortByKeysLocal(C);
    ic = ic(p);
  end
end
%=============================================================================
function [C, p] = sortByKeysLocal(vals)
  % returns vals sorted (using the builtin unique ordering) and the
  % permutation p such that C = vals(p). vals must not contain duplicates.
  sortedVals = unique(vals);
  if numel(sortedVals) ~= numel(vals)
    % cannot establish a one to one mapping: keep current order.
    C = vals;
    p = (1:numel(vals))';
    return
  end
  keys0 = valuesToKeys(vals);
  keysS = valuesToKeys(sortedVals);
  p = zeros(numel(vals), 1);
  for k = 1:numel(vals)
    p(k) = find(strcmp(keys0, keysS{k}), 1);
  end
  C = vals(p);
end
%=============================================================================
function C = reshapeLikeInputs(C, A, B)
  if isrow(A) && isrow(B)
    C = C(:).';
  else
    C = C(:);
  end
end
%=============================================================================
function keys = valuesToKeys(values)
  keys = cell(numel(values), 1);
  for k = 1:numel(values)
    keys{k} = valueKey(values(k));
  end
end
%=============================================================================
function key = valueKey(value)
  if iscell(value)
    if isempty(value)
      key = 'cell:[]';
    else
      key = ['cell:', valueKey(value{1})];
    end
  elseif isstring(value)
    key = ['string:', char(value)];
  elseif ischar(value)
    key = ['char:', value];
  elseif isnumeric(value) || islogical(value)
    key = [class(value), ':', mat2str(value)];
  else
    key = [class(value), ':', char(string(value))];
  end
end
%=============================================================================
function [tf, loc] = ismemberKeys(keysA, keysB)
  tf = false(length(keysA), 1);
  loc = zeros(length(keysA), 1);
  for k = 1:length(keysA)
    found = find(strcmp(keysB, keysA{k}), 1);
    if ~isempty(found)
      tf(k) = true;
      loc(k) = found;
    end
  end
end
%=============================================================================
