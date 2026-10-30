%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef dictionary
  properties (Hidden)
    % Native persistent store (handle). It is immutable: every mutation returns
    % a NEW store that shares structure with the old one, so copying a
    % dictionary (d2 = d) shares the handle yet keeps value-copy semantics with
    % no per-operation copy. Insert/lookup/remove are O(log N).
    store
    keyType
    valueType
    % Kept only for backward compatibility (a vestigial public field some code
    % and tests still assign); it holds no state.
    map
  end

  methods
    function obj = dictionary(varargin)
      obj.store = __dictionary_store__('new');
      obj.keyType = '';
      obj.valueType = '';
      obj.map = struct();

      if nargin == 0
        return
      end

      if nargin == 1
        inputValue = varargin{1};
        if isa(inputValue, 'py.dict')
          obj = dictionary.convertPyDictToDictionary(inputValue);
          return
        end
        if isa(inputValue, 'jl') && startsWith(inputValue.typeof, 'Dict{')
          obj = dictionary.convertJlDictToDictionary(inputValue);
          return
        end
        if ~isa(inputValue, 'dictionary')
          error('Nelson:dictionary:IncorrectNumberOfInputs', 'Incorrect number of input arguments. Each key must be followed by a value.');
        end
        % Copy: share the (immutable) store handle; a later write to either
        % dictionary builds its own new store, so the two never alias.
        obj.store = inputValue.store;
        obj.keyType = inputValue.keyType;
        obj.valueType = inputValue.valueType;
        return
      end

      if mod(nargin, 2) ~= 0
        error('Nelson:dictionary:IncorrectNumberOfInputs', 'Incorrect number of input arguments. Each key must be followed by a value.');
      end

      for p = 1:2:numel(varargin)
        keyInput = varargin{p};
        valueInput = varargin{p + 1};
        if ischar(keyInput)
          keyInput = convertCharsToStrings(keyInput);
        end
        if ischar(valueInput)
          valueInput = convertCharsToStrings(valueInput);
        end
        if numel(keyInput) ~= numel(valueInput) && numel(valueInput) ~= 1
          error('Nelson:dictionary:IncorrectNumberOfInputs', 'Incorrect number of input arguments. Each key must be followed by a value.');
        end
        if isempty(obj.keyType)
          obj.keyType = class(keyInput);
          obj.valueType = class(valueInput);
        end
        [convKeys, convValues] = dictionary.prepareEntries(obj, keyInput, valueInput);
        obj.store = __dictionary_store__(obj.store, 'set', convKeys, convValues);
      end
    end

    varargout = subsref(obj, s)
    obj = subsasgn(obj, s, val)
    varargout = insert(varargin)
    varargout = lookup(varargin)
    varargout = isKey(varargin)
    varargout = remove(varargin)
    varargout = isequal(varargin)
    varargout = isequalto(varargin)
    varargout = ndims(varargin)
    disp(varargin)
    display(varargin)

    function s = saveObjectImpl(obj)
      % Serialize by logical contents (the native store handle cannot be
      % saved); loadObjectImpl rebuilds an equivalent dictionary.
      s = struct();
      s.keyType = obj.keyType;
      s.valueType = obj.valueType;
      if isempty(obj.keyType)
        s.keys = {};
        s.values = {};
      else
        s.keys = dictionary.rawKeys(obj);
        s.values = dictionary.rawValues(obj);
      end
    end

    function n = numel(varargin)
      n = 1;
    end

    function n = length(varargin)
      n = 1;
    end

    function varargout = size(varargin)
      sz = [1 1];
      if nargin == 1
        if nargout <= 1
          varargout{1} = sz;
        else
          for k = 1:nargout
            if k <= length(sz)
              varargout{k} = sz(k);
            else
              varargout{k} = 1;
            end
          end
        end
      else
        dim = varargin{2};
        if dim <= length(sz)
          varargout{1} = sz(dim);
        else
          varargout{1} = 1;
        end
      end
    end

    function n = end(varargin)
      n = 1;
    end

    function varargout = properties(varargin)
      properties_list = {};
      if nargout == 1
        varargout{1} = properties_list;
        return
      end
      fmt = format();
      if strcmp(fmt.LineSpacing, 'loose')
        fprintf('\n')
      end
      fprintf([_('Properties for class %s:'), '\n'], class(varargin{1}))
      if strcmp(fmt.LineSpacing, 'loose')
        fprintf('\n')
      end
      if strcmp(fmt.LineSpacing, 'loose')
        fprintf('\n')
      end
    end

    function varargout = methods(varargin)
      methods_list = {'dictionary'; 'entries'; 'insert'; 'isConfigured'; 'isKey'; 'keys'; ...
        'length'; 'lookup'; 'ndims'; 'numEntries'; 'numel'; 'remove'; 'size'; ...
        'types'; 'values'};
      if nargout == 1
        varargout{1} = methods_list;
        return
      end
      fmt = format();
      if strcmp(fmt.LineSpacing, 'loose')
        fprintf('\n')
      end
      fprintf([_('Methods for class %s:'), '\n'], class(varargin{1}))
      if strcmp(fmt.LineSpacing, 'loose')
        fprintf('\n')
      end
      for e = methods_list'
        fprintf('    %s\n', e{1});
      end
      if strcmp(fmt.LineSpacing, 'loose')
        fprintf('\n')
      end
    end

    function value = struct(varargin)
      error('Nelson:dictionary:cannotConvertToStruct', 'Conversion to struct from dictionary is not possible.');
    end
  end

  methods (Static, Hidden)
    function obj = loadObjectImpl(value)
      % Rebuild a dictionary saved by saveObjectImpl.
      obj = dictionary();
      if ~isstruct(value) || ~isfield(value, 'keyType') || isempty(value.keyType)
        return
      end
      obj.keyType = char(value.keyType);
      obj.valueType = char(value.valueType);
      if isfield(value, 'keys') && ~isempty(value.keys)
        kc = value.keys(:).';
        vc = value.values(:).';
        % Re-coerce to the declared types: a serialization back-end may change
        % the stored type (matio stores a string as char), which would make the
        % stored key's keyHash disagree with a freshly-typed query key.
        isCellValue = strcmp(obj.valueType, 'cell');
        for i = 1:numel(kc)
          if ~isa(kc{i}, obj.keyType)
            kc{i} = convertDataType(obj.keyType, kc{i});
          end
          if ~isCellValue && ~isa(vc{i}, obj.valueType)
            vc{i} = convertDataType(obj.valueType, vc{i});
          end
        end
        obj.store = __dictionary_store__(obj.store, 'set', kc, vc);
      end
    end

    function obj = fromState(map, allKeys, keyType, valueType, allValues)
      obj = dictionary();
      obj.keyType = char(keyType);
      obj.valueType = char(valueType);
      if nargin < 5
        allValues = cell(size(allKeys));
      end
      if ~isempty(allKeys)
        obj.store = __dictionary_store__(obj.store, 'set', ...
          allKeys(:).', allValues(:).');
      end
    end

    function k = rawKeys(obj)
      % Stored keys as a 1xN cell, in insertion order.
      k = __dictionary_store__(obj.store, 'keys');
    end

    function v = rawValues(obj)
      % Stored values as a 1xN cell, in insertion order.
      v = __dictionary_store__(obj.store, 'values');
    end

    function n = entryCount(obj)
      n = __dictionary_store__(obj.store, 'count');
    end

    function [convKeys, convValues] = prepareEntries(obj, keyInput, valueInput)
      % Coerce a batch of key/value inputs to the configured types, returning
      % two parallel 1xN cells ready for the native store. A scalar value is
      % broadcast to every key (last value wins is handled natively).
      keyTypeName = obj.keyType;
      valueTypeName = obj.valueType;
      nIn = numel(keyInput);
      singleValue = (numel(valueInput) == 1);
      convKeys = cell(1, nIn);
      for i = 1:nIn
        currentKey = keyInput(i);
        if ~isa(currentKey, keyTypeName)
          currentKey = convertDataType(keyTypeName, currentKey);
        end
        convKeys{i} = currentKey;
      end
      convValues = cell(1, nIn);
      for i = 1:nIn
        if singleValue
          value = valueInput;
        else
          value = valueInput(i);
        end
        if ~isa(value, valueTypeName)
          value = convertDataType(valueTypeName, value);
        end
        convValues{i} = value;
      end
    end

    function convKeys = keyCell(obj, keyInput)
      % Coerce query keys to the configured key type, as a 1xN cell, so the
      % native keyHash/keyMatch are consistent with the stored keys.
      keyTypeName = obj.keyType;
      nIn = numel(keyInput);
      convKeys = cell(1, nIn);
      for i = 1:nIn
        currentKey = keyInput(i);
        if ~isa(currentKey, keyTypeName)
          currentKey = convertDataType(keyTypeName, currentKey);
        end
        convKeys{i} = currentKey;
      end
    end

    function key = normalizeStoredKey(obj, key)
      if ~strcmp(obj.keyType, 'cell') && iscell(key) && isscalar(key)
        key = key{1};
      end
      if ~isempty(obj.keyType) && ~isa(key, obj.keyType)
        key = convertDataType(obj.keyType, key);
      end
    end

    function value = normalizeStoredValue(obj, value)
      if strcmp(obj.valueType, 'cell')
        return
      end
      if iscell(value) && isscalar(value)
        value = value{1};
      end
      if ~isempty(obj.valueType) && ~isa(value, obj.valueType)
        value = convertDataType(obj.valueType, value);
      end
    end
  end

  methods (Static, Access = private)
    function obj = convertJlDictToDictionary(inputValue)
      error('Nelson:dictionary:notmanaged', 'Julia dictionary is not supported.');
    end

    function obj = convertPyDictToDictionary(inputValue)
      obj = dictionary();
      keyType = pyrun("key_type = type(next(iter(A.keys()))).__name__", "key_type", "A", inputValue);
      pythonKeyType = keyType.char();
      st = struct(inputValue);
      names = string(fieldnames(st));
      n = numel(names);
      if n == 0
        return
      end
      if strcmp(pythonKeyType, 'float')
        obj.keyType = 'double';
      else
        obj.keyType = class(names(1));
      end
      obj.valueType = class(st.(names(1)));
      if strcmp(obj.valueType, 'py.str')
        obj.valueType = 'string';
      end
      keyTypeName = obj.keyType;
      valueTypeName = obj.valueType;
      allKeys = cell(1, n);
      for i = 1:n
        key = names(i);
        if ~isa(key, keyTypeName)
          key = convertDataType(keyTypeName, key);
        end
        allKeys{i} = key;
      end
      allValues = cell(1, n);
      for i = 1:n
        value = st.(names(i));
        if numel(value) ~= 1
          value = value(i);
        end
        if ~isa(value, valueTypeName)
          value = convertDataType(valueTypeName, value);
        end
        allValues{i} = value;
      end
      obj.store = __dictionary_store__(obj.store, 'set', allKeys, allValues);
    end
  end
end
%=============================================================================
