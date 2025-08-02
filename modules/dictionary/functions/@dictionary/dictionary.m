%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function d = dictionary(varargin)
  map = struct ();
  allKeys = {};
  keyType = '';
  valueType = '';
  if (nargin == 0)
    d = createDictionary(map, allKeys, keyType, valueType);
    return
  end
  if (nargin == 1)
    obj = varargin{1};
    if isa(obj, 'py.dict')
      d = convertPyDictToDictionary(obj);
      return;
    end
    if isa(obj, 'jl') && startsWith(obj.typeof, 'Dict{')
      d = convertJlDictToDictionary(obj);
      return;
    end
    if ~isa(obj, 'dictionary');
      error('Nelson:dictionary:IncorrectNumberOfInputs', _('Incorrect number of input arguments. Each key must be followed by a value.'));
    end
    d = createDictionary(obj.map, obj.allKeys, obj.keyType, obj.valueType);
    return
  end
  if (mod (nargin, 2) ~= 0)
    error('Nelson:dictionary:IncorrectNumberOfInputs', _('Incorrect number of input arguments. Each key must be followed by a value.'));
  end
  for p = 1:2:numel (varargin)
    k = varargin{p};
    v = varargin{p+1};
    if (ischar (k))
      k = convertCharsToStrings (k);
    end
    if (ischar (v))
      v = convertCharsToStrings (v);
    end
    if (numel (k) ~= numel (v) && numel (v) ~= 1)
      error('Nelson:dictionary:IncorrectNumberOfInputs', _('Incorrect number of input arguments. Each key must be followed by a value.'));
    end
    if isempty (keyType)
      keyType = class (k);
      valueType = class (v);
    end
    for i = 1:numel(k)
      key = k(i);
      if (~isa(key, keyType))
        key = convertDataType(keyType, k(i))
      end
      h = dec2hex(keyHash(key));
      if (~isfield(map, h))
        allKeys{end + 1} = key;
      end
      if (numel(v) == 1)
        if isa(v, valueType)
          map.(h) = v;
        else
          map.(h) = convertDataType(valueType, v);
        end
      else
        if (isa(v, valueType))
          map.(h) = v(i);
        else
          map.(h) = convertDataType(valueType, v(i));
        end
      end
    end
  end
  d = createDictionary(map, allKeys, keyType, valueType);
end
%=============================================================================
function d = convertJlDictToDictionary(obj)
  error('Nelson:dictionary:notmanaged', _('Julia dictionary is not managed.'));
end
%=============================================================================
function d = convertPyDictToDictionary(obj)
  map = struct ();
  allKeys = {};
  keyType = '';
  valueType = '';
  keyType = pyrun("key_type = type(next(iter(A.keys()))).__name__", "key_type", "A", obj);
  pythonKeyType = keyType.char();
  st = struct(obj);
  names = string(fieldnames(st));
  for i = 1:numel(names)
    key = names(i);
    value = st.(key);
    if i == 1
      if strcmp(pythonKeyType, 'float') 
        keyType = 'double';
      else
        keyType = class(key);
      end
      valueType = class(value);
      if strcmp(valueType, 'py.str')
        valueType = 'string';
      end
    end
    if (~isa(key, keyType))
      key = convertDataType(keyType, key);
    end
    h = dec2hex(keyHash(key));
    if (~isfield(map, h))
      allKeys{end + 1} = key;
    end
    if (numel(value) == 1)
      if isa(value, valueType)
        map.(h) = value;
      else
        map.(h) = convertDataType(valueType, value);
      end
    else
      if (isa(value, valueType))
        map.(h) = value(i);
      else
        map.(h) = convertDataType(valueType, value(i));
      end
    end
  end
  d = createDictionary(map, allKeys, keyType, valueType);
end
%=============================================================================
function d = createDictionary(map, allKeys, keyType, valueType)
  st = struct();
  st.map = map;
  st.allKeys = allKeys;
  st.keyType = keyType;
  st.valueType = valueType;
  d = class(st, 'dictionary');
end
%=============================================================================
