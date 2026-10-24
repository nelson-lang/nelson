%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function obj = subsasgn(obj, s, val)
  if numel(s) > 1
    if strcmp(s(1).type, '.')
      currentValue = obj.(s(1).subs);
      currentValue = subsasgn(currentValue, s(2:end), val);
      obj = subsasgn(obj, s(1), currentValue);
      return
    end
    if ~strcmp(s(1).type, '()') && ~strcmp(s(1).type, '{}')
      error('Nelson:dictionary:invalidChainedAssignment', 'Chained assignment is supported only for dictionary entries.');
    end
    if numel(dictionaryIndexKey(s(1))) ~= 1
      error('Nelson:dictionary:invalidChainedAssignment', 'Chained assignment is supported only for dictionary entries.');
    end
    currentValue = subsref(obj, s(1));
    currentValue = subsasgn(currentValue, s(2:end), val);
    obj = subsasgn(obj, s(1), currentValue);
    return
  end

  switch (s(1).type)
    case '.'
      obj.(s(1).subs) = val;
    case '()'
      obj = dictionaryParenAssign(obj, s(1), val);
    case '{}'
      obj = dictionaryBraceAssign(obj, s(1), val);
    otherwise
      error('Nelson:dictionary:invalidIndexing', 'Only ''()'' and ''{}'' indexing is supported for assigning values.');
  end
end
%=============================================================================
function obj = dictionaryParenAssign(obj, s, val)
  if (iscell(s.subs) && (numel(s.subs) > 1))
    error('Nelson:dictionary:TooManyInputsToParenAssign', 'Too many input arguments. To insert multiple entries, utilize an array of keys instead.');
  end
  key = dictionaryIndexKey(s);
  if (~isConfigured(obj))
    key = dictionaryKeyForUnconfiguredDictionary(key);
    if ischar(val)
      val = string(val);
    end
    obj.keyType = class(key);
    obj.valueType = class(val);
  else
    key = dictionaryKeyForType(key, obj.keyType);
    if ischar(val)
      val = string(val);
    end
  end
  if ischar(val)
    nbValues = numel(string(val));
  else
    nbValues = numel(val);
  end
  if (numel(key) ~= nbValues && nbValues ~= 1 && nbValues ~= 0)
    error('Nelson:dictionary:keyValueCountMismatch', 'The number of keys and values must match.');
  end
  if (isa(val, 'double') && isequal(size(val), [0 0]))
    obj = dictionaryRemoveKeys(obj, key);
    return
  end
  % Delegate to the batch builder: it preserves insertion order and
  % overwrite-on-duplicate semantics while staying linear.
  [obj.allKeys, obj.allValues, obj.keyHashes] = dictionary.appendEntries(obj, key, val);
end
%=============================================================================
function obj = dictionaryBraceAssign(obj, s, val)
  if (iscell(s.subs) && (numel(s.subs) > 1))
    error('Nelson:dictionary:TooManyInputsToBracketAssign', 'Too many input arguments. To insert multiple entries, utilize an array of keys instead.');
  end
  key = dictionaryIndexKey(s);
  if (~isConfigured(obj))
    key = dictionaryKeyForUnconfiguredDictionary(key);
    obj.keyType = class(key);
    obj.valueType = 'cell';
  else
    key = dictionaryKeyForType(key, obj.keyType);
  end
  if ~strcmp(obj.valueType, 'cell')
    error('Nelson:dictionary:unsupportedBraceIndexing', 'Using curly braces on a dictionary with ''%s'' value type is not supported. The dictionary value type must be ''cell''.', obj.valueType);
  end

  nbValues = 1;
  if numel(key) > 1
    nbValues = numel(val);
  end
  if (numel(key) ~= nbValues && nbValues ~= 1)
    error('Nelson:dictionary:keyValueCountMismatch', 'The number of keys and values must match.');
  end
  nK = numel(key);
  % Build the key and wrapped-value cells in cell-only loops (key type hoisted
  % and the element helpers inlined, to avoid a per-element subsref on obj),
  % then delegate to the batch assembler. Keeps bulk brace assignment linear.
  keyTypeName = obj.keyType;
  convKeys = cell(1, nK);
  for k = 1:nK
    currentKey = key(k);
    if ~isa(currentKey, keyTypeName)
      currentKey = convertDataType(keyTypeName, currentKey);
    end
    convKeys{k} = currentKey;
  end
  multi = (nK > 1 && nbValues ~= 1);
  valIsCell = iscell(val);
  convValues = cell(1, nK);
  for k = 1:nK
    if ~multi
      content = val;
    elseif valIsCell
      content = val{k};
    else
      content = val(k);
    end
    convValues{k} = {content};
  end
  [obj.allKeys, obj.allValues, obj.keyHashes] = dictionary.assembleEntries(obj, convKeys, convValues);
end
%=============================================================================
function key = dictionaryIndexKey(s)
  if (isempty(s.subs))
    error('Nelson:dictionary:noKeySpecified', "No key specified");
  end
  key = s.subs{1};
end
%=============================================================================
function key = dictionaryKeyForUnconfiguredDictionary(key)
  if ischar(key)
    key = string(key);
  end
end
%=============================================================================
function key = dictionaryKeyForType(key, keyType)
  if strcmp(keyType, 'string') && ischar(key)
    key = string(key);
  end
end
%=============================================================================
function obj = dictionaryRemoveKeys(obj, key)
  % Resolve every index in one bulk pass, then delete in a single vectorized
  % operation per array so a cell and a numeric array are never deleted together
  % in a loop (which is quadratic in Nelson). findKeyIndices resolves all keys
  % at once (linear for numeric/string keys); a per-key findKeyIndex loop here
  % would be O(nK * nEntries).
  idx = dictionary.findKeyIndices(obj, key);
  idx = unique(idx(idx > 0));
  if isempty(idx)
    return
  end
  obj.allKeys(idx) = [];
  obj.allValues(idx) = [];
  if numel(obj.keyHashes) == numel(obj.allKeys) + numel(idx)
    obj.keyHashes(idx) = [];
  else
    obj.keyHashes = dictionary.computeKeyHashes(obj, obj.allKeys);
  end
end
%=============================================================================
