%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef Map < handle
  properties (Dependent)
    Count
    KeyType
    ValueType
  end

  properties (Hidden, Access = private)
    KeyList = []
    ValueList = []
    KeyTypeValue = 'char'
    ValueTypeValue = 'any'
    IndexMap = []
    NativeIndexMap = []
    UniformValuesFlag = true
  end

  methods
    function obj = Map(varargin)
      if nargin == 0
        __containers_map__(obj);
        return
      end

      % Options-only form beginning with 'UniformValues':
      %   containers.Map('UniformValues', tf)
      % builds an empty map. When tf is false the value type is not enforced
      % (any value may be stored under a key), so the effective value type is
      % 'any'. 'UniformValues' cannot be combined with KeyType/ValueType.
      if containers.Map.isOption(varargin{1}, 'UniformValues')
        if nargin < 2
          error('Nelson:notEnoughInputs', 'Not enough input arguments.');
        end
        if nargin > 2
          error('Nelson:tooManyInputs', 'Too many input arguments.');
        end
        obj.UniformValuesFlag = containers.Map.optionToLogical(varargin{2});
        __containers_map__(obj);
        if ~obj.UniformValuesFlag
          obj.ValueTypeValue = 'any';
        end
        return
      end

      if nargin == 4 && containers.Map.isOption(varargin{1}, 'KeyType') && containers.Map.isOption(varargin{3}, 'ValueType')
        __containers_map__(obj, varargin{:});
        return
      end

      if nargin < 2
        error('Nelson:notEnoughInputs', 'Not enough input arguments.');
      end

      if nargin == 2
        try
          __containers_map__(obj, varargin{1}, varargin{2});
          return
        catch
        end
      end

      keyCells = containers.Map.keyCellsFromInput(varargin{1});
      valueCells = containers.Map.valueCellsFromInput(varargin{2}, numel(keyCells));
      obj.KeyTypeValue = containers.Map.inferKeyType(keyCells);
      obj.ValueTypeValue = containers.Map.inferValueType(valueCells);
      if nargin > 2
        options = varargin(3:end);
        obj.parseOptions(options{:});
        if obj.UniformValuesFlag && strcmp(obj.ValueTypeValue, 'any') && ~isempty(valueCells)
          error('Nelson:containersMap:nonUniformValues', 'When ''UniformValues'' is true, values must be all character arrays or all scalars of matching type.');
        end
      end
      if ~strcmp(obj.KeyTypeValue, 'char')
        % Numeric-keyed maps use the native O(1) storage. Inserting through
        % assignOne keeps construction linear (no per-key list rebuild).
        __containers_map__(obj, '__initnative');
        for k = 1:numel(keyCells)
          obj.assignOne(keyCells{k}, valueCells{k});
        end
        return
      end
      keyList = {};
      valueList = {};
      indexMap = struct();
      for k = 1:numel(keyCells)
        if strcmp(obj.KeyTypeValue, 'char')
          if isstring(keyCells{k})
            error('Nelson:containersMap:keyTypeMismatch', 'Specified key type does not match the type expected for this container.');
          end
          key = containers.Map.normalizeCharScalar(keyCells{k});
          if ~(ischar(key) && isrow(key))
            error('Nelson:containersMap:keyTypeMismatch', 'Specified key type does not match the type expected for this container.');
          end
        else
          key = containers.Map.normalizeKeyForType(keyCells{k}, obj.KeyTypeValue);
        end
        value = containers.Map.normalizeValueForType(valueCells{k}, obj.ValueTypeValue);
        h = containers.Map.hashField(key);
        if strcmp(obj.KeyTypeValue, 'char')
          if isfield(indexMap, h)
            idx = containers.Map.findCharPosition(keyList, key);
          else
            idx = 0;
          end
        else
          idx = containers.Map.findIndexInLists(keyList, indexMap, h, key);
        end
        if idx <= 0
          keyList{end + 1} = key;
          valueList{end + 1} = value;
          if strcmp(obj.KeyTypeValue, 'char')
            indexMap.(h) = value;
          elseif isfield(indexMap, h)
            candidates = indexMap.(h);
            candidates(end + 1) = numel(keyList);
            indexMap.(h) = candidates;
          else
            indexMap.(h) = numel(keyList);
          end
        else
          valueList{idx} = value;
          if strcmp(obj.KeyTypeValue, 'char')
            indexMap.(h) = value;
          end
        end
      end
      obj.KeyList = keyList;
      obj.ValueList = valueList;
      obj.IndexMap = indexMap;
    end

    function value = get.Count(obj)
      if obj.hasNativeStorage()
        value = __containers_map__(obj, '__count');
        return
      end
      value = numel(obj.KeyList);
    end

    function set.Count(varargin)
      error('Nelson:containersMap:readOnlyProperty', 'Property ''%s'' is read-only.', 'Count');
    end

    function value = get.KeyType(obj)
      value = obj.KeyTypeValue;
    end

    function set.KeyType(varargin)
      error('Nelson:containersMap:readOnlyProperty', 'Property ''%s'' is read-only.', 'KeyType');
    end

    function value = get.ValueType(obj)
      value = obj.ValueTypeValue;
    end

    function set.ValueType(varargin)
      error('Nelson:containersMap:readOnlyProperty', 'Property ''%s'' is read-only.', 'ValueType');
    end

    function varargout = subsref(obj, s)
      switch s(1).type
        case '.'
          [varargout{1:nargout}] = builtin('subsref', obj, s);
        case '()'
          if isempty(s(1).subs)
            error('Nelson:containersMap:noKeySpecified', 'No key specified.');
          end
          if strcmp(obj.KeyTypeValue, 'char')
            value = obj.lookupCharKey(s(1).subs{1});
          else
            value = obj.lookupOne(s(1).subs{1});
          end
          if numel(s) > 1
            value = subsref(value, s(2:end));
          end
          varargout{1} = value;
        otherwise
          error('Nelson:containersMap:invalidIndexing', 'Invalid indexing.');
      end
    end

    function obj = subsasgn(obj, s, value)
      switch s(1).type
        case '.'
          obj = builtin('subsasgn', obj, s, value);
        case '()'
          if numel(s) > 1
            currentValue = obj.lookupOne(s(1).subs{1});
            currentValue = subsasgn(currentValue, s(2:end), value);
            obj.assignOne(s(1).subs{1}, currentValue);
          else
            obj.assignOne(s(1).subs{1}, value);
          end
        otherwise
          error('Nelson:containersMap:invalidIndexing', 'Invalid indexing.');
      end
    end

    function tf = isKey(obj, key)
      keyCells = containers.Map.keyCellsFromIsKeyInput(key);
      tf = false(size(keyCells));
      for k = 1:numel(keyCells)
        if strcmp(obj.KeyTypeValue, 'char') && obj.hasNativeStorage()
          currentKey = containers.Map.normalizeCharScalar(keyCells{k});
          if ischar(currentKey) && isrow(currentKey)
            tf(k) = __containers_map__(obj, '__iskey', currentKey, []);
          end
        elseif ~strcmp(obj.KeyTypeValue, 'char') && obj.hasNativeStorage()
          try
            nativeKey = containers.Map.normalizeKeyForType(keyCells{k}, obj.KeyTypeValue);
            tf(k) = __containers_map__(obj, '__iskey', nativeKey, []);
          catch
            tf(k) = false;
          end
        else
          try
            tf(k) = obj.findKeyIndex(keyCells{k}) > 0;
          catch
            tf(k) = false;
          end
        end
      end
      if iscell(key)
        tf = reshape(tf, size(key));
      end
    end

    function k = keys(obj)
      k = obj.rawKeyList();
      if numel(k) > 1
        k = k(obj.sortedKeyOrder(k));
      end
    end

    function v = values(obj, keySet)
      if nargin < 2
        v = obj.rawValueList();
        if numel(v) > 1
          v = v(obj.sortedKeyOrder(obj.rawKeyList()));
        end
        return
      end
      if ~iscell(keySet)
        error('Nelson:containersMap:invalidKeySet', 'Invalid input for argument 2 (keySet): a cell array of keys expected.');
      end
      keyCells = containers.Map.keyCellsFromInput(keySet);
      v = cell(size(keySet));
      for k = 1:numel(keyCells)
        if strcmp(obj.KeyTypeValue, 'char')
          v{k} = obj.lookupCharKey(keyCells{k});
        else
          v{k} = obj.lookupOne(keyCells{k});
        end
      end
    end

    function obj = remove(obj, key)
      keyCells = containers.Map.keyCellsFromInput(key);
      if strcmp(obj.KeyTypeValue, 'char') && obj.hasNativeStorage()
        for k = 1:numel(keyCells)
          currentKey = containers.Map.normalizeCharScalar(keyCells{k});
          if ischar(currentKey) && isrow(currentKey)
            __containers_map__(obj, '__remove', currentKey, []);
          end
        end
        return
      end
      if ~strcmp(obj.KeyTypeValue, 'char') && obj.hasNativeStorage()
        for k = 1:numel(keyCells)
          nativeKey = containers.Map.normalizeKeyForType(keyCells{k}, obj.KeyTypeValue);
          __containers_map__(obj, '__remove', nativeKey, []);
        end
        return
      end
      obj.materializeNativeStorage();
      positions = zeros(1, numel(keyCells));
      for k = 1:numel(keyCells)
        positions(k) = obj.findKeyIndex(keyCells{k});
      end
      positions = positions(positions > 0);
      if ~isempty(positions)
        obj.KeyList(positions) = [];
        obj.ValueList(positions) = [];
      end
      obj.rebuildIndex();
    end

    function obj = vertcat(varargin)
      % [m1; m2; ...]: one map holding the keys of all, later maps overriding
      % the values of the earlier ones.
      first = varargin{1};
      valueType = first.ValueType;
      for i = 1:nargin
        m = varargin{i};
        if ~isa(m, 'containers.Map') || ~strcmp(m.KeyType, first.KeyType)
          error('Nelson:containersMap:concatKeyTypeMismatch', 'All maps must have the same key type to be concatenated.');
        end
        if ~strcmp(m.ValueType, valueType)
          valueType = 'any';
        end
      end
      obj = containers.Map('KeyType', first.KeyType, 'ValueType', valueType);
      for i = 1:nargin
        ks = keys(varargin{i});
        vs = values(varargin{i});
        for k = 1:numel(ks)
          obj.assignOne(ks{k}, vs{k});
        end
      end
    end

    function n = length(obj)
      n = double(obj.Count);
    end

    function n = numel(varargin)
      n = 1;
    end

    function tf = isempty(obj)
      tf = (obj.Count == 0);
    end

    function tf = isequal(varargin)
      tf = containers.Map.allMapsEqual(varargin, false);
    end

    function tf = isequaln(varargin)
      tf = containers.Map.allMapsEqual(varargin, true);
    end

    function varargout = size(obj, dim)
      sz = [double(obj.Count), 1];
      if nargin > 1
        if dim <= 2
          varargout{1} = sz(dim);
        else
          varargout{1} = 1;
        end
        return
      end
      if nargout <= 1
        varargout{1} = sz;
      else
        for k = 1:nargout
          if k <= 2
            varargout{k} = sz(k);
          else
            varargout{k} = 1;
          end
        end
      end
    end
  end

  methods (Access = private)
    function k = rawKeyList(obj)
      if obj.hasNativeStorage()
        k = __containers_map__(obj, '__keys');
      elseif isempty(obj.KeyList)
        k = cell(1, 0);
      else
        k = obj.KeyList;
      end
    end

    function v = rawValueList(obj)
      if obj.hasNativeStorage()
        v = __containers_map__(obj, '__values');
      elseif isempty(obj.ValueList)
        v = cell(1, 0);
      else
        v = obj.ValueList;
      end
    end

    function ord = sortedKeyOrder(obj, k)
      % containers.Map returns keys (and matching values) in sorted key
      % order, regardless of insertion order.
      if strcmp(obj.KeyTypeValue, 'char')
        [~, ord] = sort(k);
      else
        [~, ord] = sort(cellfun(@(x) x, k));
      end
    end

    function parseOptions(obj, varargin)
      if mod(nargin - 1, 2) ~= 0
        error('Nelson:wrongNumberOfInputs', 'Wrong number of input arguments.');
      end
      for k = 1:2:numel(varargin)
        if containers.Map.isOption(varargin{k}, 'UniformValues')
          obj.UniformValuesFlag = containers.Map.optionToLogical(varargin{k + 1});
          if ~obj.UniformValuesFlag
            % A non-uniform map does not enforce a value type: any value may
            % be stored under a key, so the effective value type is 'any'.
            obj.ValueTypeValue = 'any';
          end
          continue
        end
        error('Nelson:containersMap:unknownOption', 'Unknown option.');
      end
    end

    function value = lookupOne(obj, key)
      if strcmp(obj.KeyTypeValue, 'char')
        value = obj.lookupCharKey(key);
        return
      end
      if obj.hasNativeStorage()
        nativeKey = containers.Map.normalizeKeyForType(key, obj.KeyTypeValue);
        value = __containers_map__(obj, '__lookup', nativeKey, []);
        return
      end
      idx = obj.findKeyIndex(key);
      if idx <= 0
        error('Nelson:Containers:Map:NoKey', 'The specified key is not present in this container.');
      end
      value = obj.ValueList{idx};
    end

    function value = lookupCharKey(obj, key)
      key = containers.Map.normalizeCharScalar(key);
      if ~(ischar(key) && isrow(key))
        error('Nelson:containersMap:keyTypeMismatch', 'Specified key type does not match the type expected for this container.');
      end
      if obj.hasNativeStorage()
        value = __containers_map__(obj, '__lookup', key, []);
        return
      end
      h = containers.Map.hashField(key);
      if ~isfield(obj.IndexMap, h)
        idx = containers.Map.findCharPosition(obj.KeyList, key);
        if idx > 0
          value = obj.ValueList{idx};
          return
        end
        error('Nelson:Containers:Map:NoKey', 'The specified key is not present in this container.');
      end
      value = obj.IndexMap.(h);
    end

    function assignOne(obj, key, value)
      if strcmp(obj.KeyTypeValue, 'char')
        key = containers.Map.normalizeCharScalar(key);
        if ~(ischar(key) && isrow(key))
          error('Nelson:containersMap:keyTypeMismatch', 'Specified key type does not match the type expected for this container.');
        end
        if obj.hasNativeStorage()
          value = containers.Map.normalizeValueForType(value, obj.ValueTypeValue);
          __containers_map__(obj, '__set', key, value);
          return
        end
      else
        key = containers.Map.normalizeKeyForType(key, obj.KeyTypeValue);
        if obj.hasNativeStorage()
          value = containers.Map.normalizeValueForType(value, obj.ValueTypeValue);
          __containers_map__(obj, '__set', key, value);
          return
        end
      end
      obj.materializeNativeStorage();
      value = containers.Map.normalizeValueForType(value, obj.ValueTypeValue);
      if strcmp(obj.KeyTypeValue, 'char')
        h = containers.Map.hashField(key);
        idx = containers.Map.findCharPosition(obj.KeyList, key);
      else
        idx = obj.findKeyIndex(key);
      end
      if idx <= 0
        keyList = obj.materializedKeyList();
        valueList = obj.materializedValueList();
        keyList{end + 1} = key;
        valueList{end + 1} = value;
        obj.KeyList = keyList;
        obj.ValueList = valueList;
        if strcmp(obj.KeyTypeValue, 'char')
          obj.IndexMap.(h) = value;
          obj.NativeIndexMap = [];
          return
        end
      else
        valueList = obj.ValueList;
        valueList{idx} = value;
        obj.ValueList = valueList;
        if strcmp(obj.KeyTypeValue, 'char')
          obj.IndexMap.(h) = value;
          obj.NativeIndexMap = [];
          return
        end
      end
      obj.rebuildIndex();
    end

    function idx = findKeyIndex(obj, key)
      idx = 0;
      key = containers.Map.normalizeKeyForType(key, obj.KeyTypeValue);
      if strcmp(obj.KeyTypeValue, 'char')
        if obj.hasNativeStorage()
          idx = double(__containers_map__(obj, '__iskey', key, []));
          return
        end
        h = containers.Map.hashField(key);
        if ~isfield(obj.IndexMap, h)
          idx = containers.Map.findCharPosition(obj.KeyList, key);
          return
        end
        idx = containers.Map.findCharPosition(obj.KeyList, key);
        return
      end
      h = containers.Map.hashField(key);
      if ~isfield(obj.IndexMap, h)
        return
      end
      candidates = obj.IndexMap.(h);
      for k = 1:numel(candidates)
        candidateIndex = candidates(k);
        if containers.Map.keysEqual(obj.KeyList{candidateIndex}, key)
          idx = candidateIndex;
          return
        end
      end
    end

    function rebuildIndex(obj)
      indexMap = struct();
      if isempty(obj.KeyList)
        obj.IndexMap = indexMap;
        obj.NativeIndexMap = [];
        return
      end
      for k = 1:numel(obj.KeyList)
        h = containers.Map.hashField(obj.KeyList{k});
        if strcmp(obj.KeyTypeValue, 'char')
          indexMap.(h) = obj.ValueList{k};
        elseif isfield(indexMap, h)
          candidates = indexMap.(h);
          candidates(end + 1) = k;
          indexMap.(h) = candidates;
        else
          indexMap.(h) = k;
        end
      end
      obj.IndexMap = indexMap;
      obj.NativeIndexMap = [];
    end

    function tf = hasNativeStorage(obj)
      tf = ~isempty(obj.NativeIndexMap);
    end

    function materializeNativeStorage(obj)
      if ~obj.hasNativeStorage()
        return
      end
      obj.KeyList = __containers_map__(obj, '__keys');
      obj.ValueList = __containers_map__(obj, '__values');
      obj.rebuildIndex();
    end

    function list = materializedKeyList(obj)
      list = obj.KeyList;
      if isempty(list)
        list = cell(1, 0);
      end
    end

    function list = materializedValueList(obj)
      list = obj.ValueList;
      if isempty(list)
        list = cell(1, 0);
      end
    end
  end

  methods (Static, Access = private)
    function tf = allMapsEqual(args, nanEqual)
      % Two maps are equal when they hold the same keys and, in sorted key
      % order, equal values. keys() and values() return matching orders.
      tf = true;
      first = args{1};
      if ~isa(first, 'containers.Map')
        tf = false;
        return
      end
      for i = 2:numel(args)
        other = args{i};
        if ~isa(other, 'containers.Map') || first.Count ~= other.Count
          tf = false;
          return
        end
        if ~isequal(keys(first), keys(other))
          tf = false;
          return
        end
        if nanEqual
          equalValues = isequaln(values(first), values(other));
        else
          equalValues = isequal(values(first), values(other));
        end
        if ~equalValues
          tf = false;
          return
        end
      end
    end

    function tf = isOption(value, name)
      if isstring(value) && isscalar(value)
        value = char(value);
      end
      tf = ischar(value) && strcmpi(value, name);
    end

    function tf = optionToLogical(value)
      if islogical(value) && isscalar(value)
        tf = value;
        return
      end
      if isnumeric(value) && isscalar(value)
        tf = (value ~= 0);
        return
      end
      error('Nelson:validators:mustBeLogicalScalar', '%s must be a logical scalar.', 'UniformValues');
    end

    function typeName = normalizeTypeName(value)
      if isstring(value) && isscalar(value)
        value = char(value);
      end
      if ~ischar(value)
        error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Type name');
      end
      typeName = lower(value);
    end

    function keyCells = keyCellsFromInput(key)
      if iscell(key)
        keyCells = key(:).';
      elseif ischar(key)
        keyCells = {key};
      elseif isstring(key)
        keyCells = cell(1, numel(key));
        for k = 1:numel(key)
          keyCells{k} = char(key(k));
        end
      else
        keyCells = cell(1, numel(key));
        for k = 1:numel(key)
          keyCells{k} = key(k);
        end
      end
    end

    function keyCells = keyCellsFromIsKeyInput(key)
      if iscell(key)
        keyCells = key(:).';
        return
      end
      if ischar(key) || (isstring(key) && isscalar(key))
        keyCells = {key};
        return
      end
      if isscalar(key)
        keyCells = {key};
        return
      end
      error('Nelson:containersMap:invalidKeySet', 'Invalid input. KeySet must be a numeric scalar, character vector, string scalar or cell array of keys.');
    end

    function valueCells = valueCellsFromInput(value, keyCount)
      if iscell(value)
        raw = value(:).';
      elseif ischar(value)
        raw = {value};
      else
        raw = cell(1, numel(value));
        for k = 1:numel(value)
          raw{k} = value(k);
        end
      end
      if numel(raw) == 1 && keyCount > 1 && ~iscell(value)
        valueCells = cell(1, keyCount);
        for k = 1:keyCount
          valueCells{k} = raw{1};
        end
        return
      end
      if numel(raw) ~= keyCount
        error('Nelson:containersMap:keyValueCountMismatch', 'The number of keys and values must match.');
      end
      valueCells = raw;
    end

    function typeName = inferKeyType(keyCells)
      if isempty(keyCells)
        typeName = 'char';
        return
      end
      if isstring(keyCells{1})
        error('Nelson:containersMap:keyTypeMismatch', 'Specified key type does not match the type expected for this container.');
      end
      first = containers.Map.normalizeCharScalar(keyCells{1});
      if ischar(first)
        typeName = 'char';
      else
        typeName = class(first);
        if strcmp(typeName, 'logical') || strcmp(typeName, 'int8') || strcmp(typeName, 'uint8') ...
            || strcmp(typeName, 'int16') || strcmp(typeName, 'uint16')
          typeName = 'double';
        end
      end
    end

    function typeName = inferValueType(valueCells)
      if isempty(valueCells)
        typeName = 'any';
        return
      end
      first = valueCells{1};
      firstClass = class(first);
      if ~(ischar(first) || ((isnumeric(first) || islogical(first)) && isscalar(first)))
        typeName = 'any';
        return
      end
      for k = 2:numel(valueCells)
        current = valueCells{k};
        if ~strcmp(class(current), firstClass) || (~ischar(current) && ~isscalar(current))
          typeName = 'any';
          return
        end
      end
      typeName = firstClass;
    end

    function value = normalizeCharScalar(value)
      if isstring(value) && isscalar(value)
        value = char(value);
      end
    end

    function key = normalizeKeyForType(key, typeName)
      if strcmp(typeName, 'char')
        key = containers.Map.normalizeCharScalar(key);
        if ~(ischar(key) && isrow(key))
          error('Nelson:containersMap:keyTypeMismatch', 'Specified key type does not match the type expected for this container.');
        end
        return
      end
      if ischar(key) || isstring(key) || iscell(key)
        error('Nelson:containersMap:keyTypeMismatch', 'Specified key type does not match the type expected for this container.');
      end
      if ~isa(key, typeName)
        try
          key = cast(key, typeName);
        catch
          error('Nelson:containersMap:keyTypeMismatch', 'Specified key type does not match the type expected for this container.');
        end
      end
      if ~isscalar(key)
        error('Nelson:containersMap:keyTypeMismatch', 'Specified key type does not match the type expected for this container.');
      end
      if isnumeric(key) && isnan(key)
        error('Nelson:containersMap:nanKey', 'NaN-valued key is not allowed for this container.');
      end
    end

    function value = normalizeValueForType(value, typeName)
      if strcmp(typeName, 'any')
        return
      end
      if strcmp(typeName, 'char')
        value = containers.Map.normalizeCharScalar(value);
        if ischar(value)
          return
        end
      end
      if isa(value, typeName)
        return
      end
      try
        value = cast(value, typeName);
      catch
        error('Nelson:containersMap:valueTypeMismatch', 'Specified value type does not match the type expected for this container.');
      end
    end

    function h = hashField(key)
      if ischar(key)
        h = containers.Map.charHashField(key);
        return
      end
      h = ['k', dec2hex(keyHash(key))];
    end

    function h = charHashField(key)
      h = 'c';
      for k = 1:length(key)
        h = [h, '_', dec2hex(double(key(k)), 4)];
      end
    end

    function tf = keysEqual(a, b)
      try
        tf = keyMatch(a, b);
      catch
        tf = isequal(a, b);
      end
    end

    function idx = findCharPosition(keyList, key)
      idx = 0;
      for k = 1:numel(keyList)
        if strcmp(keyList{k}, key)
          idx = k;
          return
        end
      end
    end

    function idx = findIndexInLists(keyList, indexMap, h, key)
      idx = 0;
      if ~isfield(indexMap, h)
        return
      end
      candidates = indexMap.(h);
      for k = 1:numel(candidates)
        candidateIndex = candidates(k);
        if containers.Map.keysEqual(keyList{candidateIndex}, key)
          idx = candidateIndex;
          return
        end
      end
    end

  end
end
%=============================================================================
