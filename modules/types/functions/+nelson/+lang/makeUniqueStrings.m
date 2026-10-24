%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [stringsOut, modified] = makeUniqueStrings(S, excludes, maxStringLength)
  narginchk(1, 3);
  nargoutchk(0, 2);

  [items, shape, kind] = localTextList(S);
  count = numel(items);
  inputWasChar = strcmp(kind, 'char');
  if nargin < 2
    excluded = {};
    selected = true(1, count);
  else
    [excluded, selected] = localExcludesOrSelection(excludes, count);
  end
  if nargin < 3
    maxStringLength = namelengthmax();
  else
    maxStringLength = localMaxStringLength(maxStringLength);
  end

  stringsOut = items;
  modified = false(1, count);
  used = excluded(:)';
  for k = 1:count
    if ~selected(k)
      used{end + 1} = items{k};
    end
  end
  % Fast path: when the selected names are already mutually unique and disjoint
  % from the reserved names, no suffixing happens, so assign them directly. The
  % general loop below tests membership against a growing list with
  % any(strcmp(...)), which is O(count^2) even when every name is distinct (the
  % common case, e.g. Var1..VarN from array2table).
  selIdx = find(selected);
  sel = items(selIdx);
  % Truncate using a single vectorised length computation. Calling the
  % localTruncate helper per element routes through a function whose body
  % references length(), which is pathologically slow per call in the
  % interpreter and made truncation alone O(count^2).
  truncFlag = cellfun('length', sel) > maxStringLength;
  truncated = sel;
  tooLong = find(truncFlag);
  for j = 1:numel(tooLong)
    idx = tooLong(j);
    truncated{idx} = truncated{idx}(1:maxStringLength);
  end
  if numel(unique(truncated)) == numel(truncated) && (isempty(used) || ~any(ismember(truncated, used)))
    for i = 1:numel(selIdx)
      stringsOut{selIdx(i)} = truncated{i};
      modified(selIdx(i)) = truncFlag(i);
    end
  else
    for k = 1:count
      if selected(k)
        [stringsOut{k}, wasTruncated] = localTruncate(items{k}, maxStringLength);
        stringsOut{k} = localUniqueName(stringsOut{k}, used, maxStringLength);
        used{end + 1} = stringsOut{k};
        modified(k) = wasTruncated || ~strcmp(stringsOut{k}, items{k});
      end
    end
  end

  if inputWasChar && maxStringLength == 0
    stringsOut = '';
    modified = true;
    return
  end
  stringsOut = localFromTextList(stringsOut, shape, kind);
  modified = reshape(modified, shape);
end
%=============================================================================
function [value, wasTruncated] = localTruncate(value, maxStringLength)
  wasTruncated = length(value) > maxStringLength;
  if wasTruncated
    value = value(1:maxStringLength);
  end
end
%=============================================================================
function value = localUniqueName(value, used, maxStringLength)
  if ~localIsUsed(value, used)
    return
  end
  base = value;
  suffixIndex = 1;
  while true
    suffix = ['_', int2str(suffixIndex)];
    if length(suffix) > maxStringLength
      error('Nelson:makeUniqueStrings:unableMakeStringsUniqueWithin2', 'Unable to make strings unique within the maximum string length.');
    end
    baseLength = maxStringLength - length(suffix);
    candidate = [base(1:min(length(base), baseLength)), suffix];
    if ~localIsUsed(candidate, used)
      value = candidate;
      return
    end
    suffixIndex = suffixIndex + 1;
  end
end
%=============================================================================
function [excluded, selected] = localExcludesOrSelection(value, count)
  if isnumeric(value) || islogical(value)
    excluded = {};
    selected = localSelectedMask(value, count);
  else
    excluded = localExcludedTextList(value);
    selected = true(1, count);
  end
end
%=============================================================================
function tf = localIsUsed(value, used)
  tf = false;
  if ~isempty(used)
    tf = any(strcmp(value, used));
  end
end
%=============================================================================
function selected = localSelectedMask(whichStringsIdx, count)
  selected = false(1, count);
  if islogical(whichStringsIdx)
    if numel(whichStringsIdx) ~= count
      error('Nelson:makeUniqueStrings:logicalIndexNumberElementsInput2', 'Logical index must have the same number of elements as the input.');
    end
    selected = logical(whichStringsIdx(:)');
  elseif isnumeric(whichStringsIdx)
    idx = whichStringsIdx(:)';
    if isempty(idx)
      return
    end
    if any(idx ~= floor(idx)) || any(idx < 1) || any(idx > count)
      error('Nelson:makeUniqueStrings:indexValuesPositiveIntegersWithin2', 'Index values must be positive integers within the input size.');
    end
    selected(idx) = true;
  else
    error('Nelson:makeUniqueStrings:indicesNumericLogical2', 'Indices must be numeric or logical.');
  end
end
%=============================================================================
function value = localMaxStringLength(value)
  if ~isnumeric(value) || ~isscalar(value) || value ~= floor(value) || value < 0
    error('Nelson:makeUniqueStrings:maximumStringLengthANonnegative2', 'Maximum string length must be a nonnegative integer scalar.');
  end
  value = double(value);
end
%=============================================================================
function items = localExcludedTextList(value)
  if isempty(value)
    items = {};
    return
  end
  [items, ~, ~] = localTextList(value);
end
%=============================================================================
function [items, shape, kind] = localTextList(value)
  shape = size(value);
  if ischar(value)
    if size(value, 1) ~= 1 && ~isempty(value)
      error('Nelson:validators:mustBeRow', '%s must be a row vector.', 'Character input');
    end
    items = {value};
    shape = [1 1];
    kind = 'char';
  elseif isstring(value)
    items = cellstr(value);
    kind = 'string';
  elseif iscellstr(value)
    items = value;
    kind = 'cell';
  else
    error('Nelson:types:stringArrayCharacterVectorCell2', 'String array, character vector, or cell array of character vectors expected.');
  end
  items = items(:)';
end
%=============================================================================
function out = localFromTextList(items, shape, kind)
  switch kind
    case 'string'
      out = reshape(string(items), shape);
    case 'cell'
      out = reshape(items, shape);
    otherwise
      out = items{1};
  end
end
%=============================================================================
