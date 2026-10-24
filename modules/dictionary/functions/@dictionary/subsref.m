%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = subsref(varargin)
  narginchk(2, 2);
  obj = varargin{1};
  s = varargin{2};

  switch (s(1).type)
    case '.'
      [varargout{1:nargout}] = dictionaryDotReference(obj, s);
    case '()'
      value = dictionaryParenReference(obj, s(1));
      if numel(s) > 1
        value = subsref(value, s(2:end));
      end
      varargout{1} = value;
    case '{}'
      contents = dictionaryBraceReference(obj, s(1));
      if numel(s) > 1
        for k = 1:numel(contents)
          contents{k} = subsref(contents{k}, s(2:end));
        end
      end
      if nargout <= 1
        if isempty(contents)
          varargout{1} = [];
        else
          varargout{1} = contents{1};
        end
      else
        varargout = contents;
      end
    otherwise
      error('Nelson:dictionary:unsupportedIndexing', '''%s'' indexing is not supported.', s(1).type);
  end
end
%=============================================================================
function varargout = dictionaryDotReference(obj, s)
  extra = {};
  if (numel(s) > 1 && strcmp(s(2).type, '()'))
    if (~isempty(s(2).subs))
      extra = s(2).subs;
    end
    s(2) = [];
  end
  switch (s(1).subs)
    case 'map'
      value = obj.map;
    case 'allKeys'
      value = obj.allKeys;
    case 'allValues'
      value = obj.allValues;
    case 'keyType'
      value = obj.keyType;
    case 'valueType'
      value = obj.valueType;
    case 'keys'
      value = keys(obj, extra{:});
    case 'values'
      value = values(obj, extra{:});
    case 'types'
      value = types(obj, extra{:});
    case 'numEntries'
      value = numEntries(obj, extra{:});
    case 'isConfigured'
      value = isConfigured(obj, extra{:});
    case 'isKey'
      value = isKey(obj, extra{:});
    otherwise
      error('Nelson:dictionary:unknownProperty', 'Unknown property: %s', s(1).subs);
  end
  if numel(s) > 1
    value = subsref(value, s(2:end));
  end
  varargout{1} = value;
end
%=============================================================================
function value = dictionaryParenReference(obj, s)
  rawValues = dictionaryValuesForKeys(obj, s);
  if strcmp(obj.valueType, 'cell')
    value = cell(size(rawValues));
    for k = 1:numel(rawValues)
      value{k} = rawValues{k}{1};
    end
    return
  end
  try
    value = [rawValues{:}];
  catch
    error('Nelson:dictionary:cannotConcatenateValues', 'Cannot concatenate values.');
  end
  key = dictionaryKeyForType(dictionaryIndexKey(s), obj.keyType);
  value = reshape(value, size(key));
end
%=============================================================================
function contents = dictionaryBraceReference(obj, s)
  if (~isConfigured(obj))
    error('Nelson:dictionary:notConfigured', "Dictionary not configured");
  end
  if ~strcmp(obj.valueType, 'cell')
    error('Nelson:dictionary:unsupportedBraceIndexing', 'Using curly braces on a dictionary with ''%s'' value type is not supported. The dictionary value type must be ''cell''.', obj.valueType);
  end
  rawValues = dictionaryValuesForKeys(obj, s);
  contents = cell(size(rawValues));
  for k = 1:numel(rawValues)
    contents{k} = rawValues{k}{1};
  end
end
%=============================================================================
function rawValues = dictionaryValuesForKeys(obj, s)
  if (isempty(s.subs))
    error('Nelson:dictionary:noKeySpecified', "No key specified");
  end
  if (iscell(s.subs) && (numel(s.subs) > 1))
    error('Nelson:dictionary:TooManyInputsToParenReference', 'Too many input arguments. To search for multiple entries, utilize an array of keys instead.');
  end
  if (~isConfigured(obj))
    error('Nelson:dictionary:notConfigured', "Dictionary not configured");
  end
  key = dictionaryIndexKey(s);
  key = dictionaryKeyForType(key, obj.keyType);
  nKeys = numel(key);
  % Phase 1: resolve the entry index of every requested key. This loop only
  % writes a numeric array; mixing a cell write with the numeric work that
  % findKeyIndex performs would turn the lookup quadratic in Nelson.
  % Resolve every requested index in one vectorized pass; a per-key
  % findKeyIndex loop is quadratic because each call scans all stored keys.
  indices = dictionary.findKeyIndices(obj, key);
  if any(indices == 0)
    error('Nelson:dictionary:unknownKey', "Unknown key");
  end
  % Phase 2: gather the stored values (vectorized) and normalize them in a
  % cell-only loop. normalizeStoredValue is inlined with the value type
  % hoisted out: calling a static method that reads the object inside a
  % cell-writing loop is quadratic in Nelson.
  rawValues = cell(size(key));
  if nKeys > 0
    storedValues = obj.allValues;
    gathered = storedValues(indices);
    valueType = obj.valueType;
    isCellValueType = strcmp(valueType, 'cell');
    emptyValueType = isempty(valueType);
    for k = 1:nKeys
      value = gathered{k};
      if ~isCellValueType
        if iscell(value) && isscalar(value)
          value = value{1};
        end
        if ~emptyValueType && ~isa(value, valueType)
          value = convertDataType(valueType, value);
        end
      end
      rawValues{k} = value;
    end
  end
end
%=============================================================================
function key = dictionaryIndexKey(s)
  key = s.subs{1};
end
%=============================================================================
function key = dictionaryKeyForType(key, keyType)
  if strcmp(keyType, 'string') && ischar(key)
    key = string(key);
  end
end
%=============================================================================
