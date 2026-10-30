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

  % Fast path for the hot scalar value lookup d(key): everything inline at the
  % method level (direct property reads, no static-method or helper dispatch),
  % which is where almost all of the per-lookup cost used to go.
  if numel(s) == 1 && strcmp(s(1).type, '()') && numel(s(1).subs) == 1
    kt = obj.keyType;
    if isempty(kt)
      error('Nelson:dictionary:notConfigured', "Dictionary not configured");
    end
    key = s(1).subs{1};
    if strcmp(kt, 'string') && ischar(key)
      key = string(key);
    end
    nKeys = numel(key);
    kc = cell(1, nKeys);
    for k = 1:nKeys
      kk = key(k);
      if ~isa(kk, kt)
        kk = convertDataType(kt, kk);
      end
      kc{k} = kk;
    end
    gathered = __dictionary_store__(obj.store, 'get', kc);
    vt = obj.valueType;
    if strcmp(vt, 'cell')
      value = cell(size(key));
      for k = 1:nKeys
        value{k} = gathered{k}{1};
      end
      varargout{1} = value;
      return
    end
    isEmptyVt = isempty(vt);
    for k = 1:nKeys
      v = gathered{k};
      if iscell(v) && isscalar(v)
        v = v{1};
      end
      if ~isEmptyVt && ~isa(v, vt)
        v = convertDataType(vt, v);
      end
      gathered{k} = v;
    end
    try
      value = [gathered{:}];
    catch
      error('Nelson:dictionary:cannotConcatenateValues', 'Cannot concatenate values.');
    end
    varargout{1} = reshape(value, size(key));
    return
  end

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
      value = dictionary.rawKeys(obj);
    case 'allValues'
      value = dictionary.rawValues(obj);
    case 'store'
      value = obj.store;
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
  % Resolve every requested value in one native O(log N)-per-key pass; the
  % store errors on an unknown key, matching the previous behavior.
  gathered = __dictionary_store__(obj.store, 'get', dictionary.keyCell(obj, key));
  % Normalize the gathered values in a cell-only loop; the value type is
  % hoisted out of the loop.
  rawValues = cell(size(key));
  if nKeys > 0
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
