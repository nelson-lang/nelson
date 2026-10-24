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
    map
    allKeys
    allValues
    keyType
    valueType
    keyHashes
  end

  methods
    function obj = dictionary(varargin)
      obj.map = struct();
      obj.allKeys = {};
      obj.allValues = {};
      obj.keyType = '';
      obj.valueType = '';
      obj.keyHashes = uint64([]);

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
        obj.map = inputValue.map;
        obj.allKeys = inputValue.allKeys;
        obj.allValues = inputValue.allValues;
        obj.keyType = inputValue.keyType;
        obj.valueType = inputValue.valueType;
        obj.keyHashes = inputValue.keyHashes;
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

        [obj.allKeys, obj.allValues, obj.keyHashes] = ...
          dictionary.appendEntries(obj, keyInput, valueInput);
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
    function obj = fromState(map, allKeys, keyType, valueType, allValues)
      obj = dictionary();
      obj.map = map;
      obj.allKeys = allKeys;
      if nargin < 5
        % The map field is vestigial and always empty, so there are no stored
        % values to recover; cell(size(...)) yields [] elements, as before.
        obj.allValues = cell(size(allKeys));
      else
        obj.allValues = allValues;
      end
      obj.keyType = keyType;
      obj.valueType = valueType;
      obj.keyHashes = dictionary.computeKeyHashes(obj, obj.allKeys);
    end

    function idx = findKeyIndex(obj, key)
      idx = 0;
      n = numel(obj.allKeys);
      if n == 0
        return
      end
      if dictionary.useHashIndex(obj)
        % O(1) average path: match the query key hash against the parallel
        % keyHashes vector, then confirm the candidate(s) with keyMatch to
        % guard against hash collisions.
        candidates = find(obj.keyHashes == dictionary.hashOf(obj, key));
        for ci = 1:numel(candidates)
          k = candidates(ci);
          storedKey = dictionary.normalizeStoredKey(obj, obj.allKeys{k});
          if dictionary.keysMatch(storedKey, key)
            idx = k;
            return
          end
        end
        return
      end
      % Fallback linear scan: logical/unknown key types (unstable hash) or a
      % keyHashes vector that is out of sync with allKeys (legacy state).
      for k = 1:n
        storedKey = dictionary.normalizeStoredKey(obj, obj.allKeys{k});
        if dictionary.keysMatch(storedKey, key)
          idx = k;
          return
        end
      end
    end

    function idx = findKeyIndices(obj, keys)
      % Bulk form of findKeyIndex: resolve the entry index of every query key
      % in one pass, returning a column of indices (0 where a key is absent).
      % For numeric and string key types the stored keys concatenate into a
      % plain array, so a single C-level ismember replaces the per-key hash
      % scan that findKeyIndex performs; a per-key loop here is quadratic.
      % Any other key type, or an unexpected stored layout, falls back to the
      % exact per-key findKeyIndex path so the result is always identical.
      nKeys = numel(keys);
      idx = zeros(nKeys, 1);
      n = numel(obj.allKeys);
      if nKeys == 0 || n == 0
        return
      end
      keyTypeName = obj.keyType;
      valueMatchable = strcmp(keyTypeName, 'string') || any(strcmp(keyTypeName, ...
        {'double', 'single', 'int8', 'uint8', 'int16', 'uint16', ...
        'int32', 'uint32', 'int64', 'uint64'}));
      if valueMatchable
        if strcmp(keyTypeName, 'string')
          % N-ary horzcat of the stored string scalars is pairwise and so
          % O(numKeys^2); string() gathers the cell into a string array in one
          % native pass, which made every lookup/isKey quadratic.
          storedVals = string(obj.allKeys);
        else
          storedVals = horzcat(obj.allKeys{:});
        end
        if isa(storedVals, keyTypeName) && numel(storedVals) == n
          query = keys;
          if ~isa(query, keyTypeName)
            query = convertDataType(keyTypeName, query);
          end
          [~, loc] = ismember(query(:), storedVals(:));
          idx = loc(:);
          return
        end
      end
      % Fallback: non-value-matchable key type (char/cell/struct/
      % function_handle/logical) or an unexpected stored-key layout.
      for k = 1:nKeys
        currentKey = keys(k);
        if ~isa(currentKey, keyTypeName)
          currentKey = convertDataType(keyTypeName, currentKey);
        end
        idx(k) = dictionary.findKeyIndex(obj, currentKey);
      end
    end

    function tf = useHashIndex(obj)
      tf = numel(obj.keyHashes) == numel(obj.allKeys) ...
        && dictionary.isHashableKeyType(obj.keyType);
    end

    function tf = isHashableKeyType(keyType)
      % keyHash is value-deterministic for these built-in types. 'logical'
      % (and user-defined classes we have not validated) fall back to the
      % linear scan; a logical-keyed dictionary holds at most two entries.
      tf = any(strcmp(keyType, {'double', 'single', 'int8', 'uint8', ...
        'int16', 'uint16', 'int32', 'uint32', 'int64', 'uint64', ...
        'char', 'string', 'cell', 'struct', 'function_handle'}));
    end

    function h = hashOf(obj, key)
      h = keyHash(dictionary.normalizeStoredKey(obj, key));
    end

    function hashes = computeKeyHashes(obj, keyCell)
      % Hash every key in one vectorized pass. cellfun gathers the keyHash
      % results in native code, which is the whole point: an element-by-element
      % hashes(k) = keyHash(...) loop is quadratic. keyHash re-enters the
      % evaluator (it resolves a possible overload), and an indexed assignment
      % whose right-hand side re-enters the evaluator forces the destination
      % array to be copied on every iteration -- O(N^2) for N keys. The stored
      % keys are already normalized, so hashing them directly is identical to
      % hashOf (which normalizes first).
      if isempty(keyCell)
        hashes = uint64([]);
        return
      end
      hashes = cellfun(@keyHash, keyCell);
      hashes = hashes(:);
    end

    function tf = keysMatch(a, b)
      try
        tf = keyMatch(a, b);
      catch
        tf = isequal(a, b);
      end
    end

    function [outKeys, outValues, outHashes] = appendEntries(obj, keyInput, valueInput)
      % Append key/value pairs to the object's entry arrays, preserving
      % insertion order and overwrite-on-duplicate (last value wins)
      % semantics. The work is split into phase-separated loops: writing a
      % cell array and a numeric array in the same loop is quadratic in
      % Nelson, so keys/values (cells) and hashes (numeric) are never grown
      % together.
      keyTypeName = obj.keyType;
      valueTypeName = obj.valueType;
      nIn = numel(keyInput);
      singleValue = (numel(valueInput) == 1);

      % Phase 1: convert incoming keys to the key type (cell writes only).
      convKeys = cell(1, nIn);
      for i = 1:nIn
        currentKey = keyInput(i);
        if ~isa(currentKey, keyTypeName)
          currentKey = convertDataType(keyTypeName, currentKey);
        end
        convKeys{i} = currentKey;
      end

      % Phase 2: cast incoming values (cell writes only). The cast is inlined
      % with convertDataType rather than a qualified static-method call, which
      % is very slow in a loop.
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

      [outKeys, outValues, outHashes] = dictionary.assembleEntries(obj, convKeys, convValues);
    end

    function [outKeys, outValues, outHashes] = assembleEntries(obj, convKeys, convValues)
      % Resolve (already converted) key/value cells into the object's entry
      % arrays. Keys are the cell convKeys, values the cell convValues.
      hashable = dictionary.isHashableKeyType(obj.keyType) ...
        && numel(obj.keyHashes) == numel(obj.allKeys);
      if ~hashable
        [outKeys, outValues, outHashes] = ...
          dictionary.appendEntriesLinear(obj, convKeys, convValues);
        return
      end
      % Hash the incoming keys. The keys were normalized by appendEntries, so
      % computeKeyHashes (which hashes them directly, in one vectorized pass)
      % matches hashOf; a per-element hashesIn(i) = keyHash(...) loop here is
      % quadratic (see computeKeyHashes for why).
      hashesIn = dictionary.computeKeyHashes(obj, convKeys);
      if isempty(obj.allKeys)
        [outKeys, outValues, outHashes] = ...
          dictionary.appendEntriesFresh(obj, convKeys, convValues, hashesIn);
      else
        [outKeys, outValues, outHashes] = ...
          dictionary.appendEntriesInto(obj, convKeys, convValues, hashesIn);
      end
    end

    function [outKeys, outValues, outHashes] = appendEntriesFresh(obj, convKeys, convValues, hashesIn)
      % Build a fresh set of entries from scratch, hashes already computed.
      nIn = numel(convKeys);
      [uniqueHashes, firstIdx, groupId] = unique(hashesIn, 'stable');
      nUnique = numel(firstIdx);
      if nUnique < nIn && dictionary.hasKeyCollision(obj, convKeys, groupId, nUnique)
        % Genuine 64-bit hash collision between distinct keys: fall back to
        % the exact linear resolution (astronomically rare).
        [outKeys, outValues, outHashes] = ...
          dictionary.appendEntriesLinear(obj, convKeys, convValues);
        return
      end
      % Each hash group is one entry; order is first appearance.
      outKeys = convKeys(firstIdx);
      outHashes = uniqueHashes(:);
      % Last value within a group wins (cell writes only).
      outValues = cell(1, nUnique);
      for i = 1:nIn
        outValues{groupId(i)} = convValues{i};
      end
    end

    function tf = hasKeyCollision(obj, convKeys, groupId, nUnique)
      % A repeated hash is almost always a repeated key; confirm that the
      % members of every multi-member group are really keyMatch-equal.
      tf = false;
      counts = accumarray(groupId, 1, [nUnique, 1]);
      for g = find(counts > 1).'
        members = find(groupId == g);
        firstKey = dictionary.normalizeStoredKey(obj, convKeys{members(1)});
        for mi = 2:numel(members)
          other = dictionary.normalizeStoredKey(obj, convKeys{members(mi)});
          if ~dictionary.keysMatch(firstKey, other)
            tf = true;
            return
          end
        end
      end
    end

    function [outKeys, outValues, outHashes] = appendEntriesInto(obj, convKeys, convValues, hashesIn)
      % Merge a new batch of pairs into an object that already holds entries.
      [fKeys, fValues, fHashes] = ...
        dictionary.appendEntriesFresh(obj, convKeys, convValues, hashesIn);
      existKeys = obj.allKeys;
      existValues = obj.allValues;
      existHashes = obj.keyHashes;
      nF = numel(fKeys);
      % Locate each new entry in the existing set. The hash vectors are already
      % computed, so one bulk ismember resolves every candidate position; a
      % per-new-key find() scan over existHashes would be O(nF * nExist). Each
      % hash hit is confirmed with keyMatch. A 64-bit hash collision between
      % distinct keys is astronomically rare; only then is the find() scan used,
      % and only for the colliding key.
      [hasMatch, matchLoc] = ismember(fHashes, existHashes);
      targetSlot = zeros(nF, 1);
      for j = 1:nF
        if ~hasMatch(j)
          continue
        end
        c = matchLoc(j);
        if dictionary.keysMatch(dictionary.normalizeStoredKey(obj, existKeys{c}), fKeys{j})
          targetSlot(j) = c;
          continue
        end
        candidates = find(existHashes == fHashes(j));
        for ci = 1:numel(candidates)
          c = candidates(ci);
          if dictionary.keysMatch(dictionary.normalizeStoredKey(obj, existKeys{c}), fKeys{j})
            targetSlot(j) = c;
            break
          end
        end
      end
      % Apply overwrites (cell writes to values only).
      outValues = existValues;
      for j = 1:nF
        if targetSlot(j) > 0
          outValues{targetSlot(j)} = fValues{j};
        end
      end
      % Append the genuinely new entries; cells and the numeric hash vector
      % are concatenated separately, never grown together in a loop.
      isNew = (targetSlot == 0);
      outKeys = [existKeys, fKeys(isNew)];
      outValues = [outValues, fValues(isNew)];
      outHashes = [existHashes; fHashes(isNew)];
    end

    function [outKeys, outValues, outHashes] = appendEntriesLinear(obj, convKeys, convValues)
      % Exact linear-scan resolution for key types whose hash is not usable
      % (e.g. logical) or in the rare event of a hash collision. Keys and
      % values (cells) are built here; the hash vector is computed in a
      % separate numeric-only pass afterwards.
      outKeys = obj.allKeys;
      outValues = obj.allValues;
      for i = 1:numel(convKeys)
        currentKey = convKeys{i};
        keyIndex = 0;
        for kk = 1:numel(outKeys)
          if dictionary.keysMatch(dictionary.normalizeStoredKey(obj, outKeys{kk}), currentKey)
            keyIndex = kk;
            break
          end
        end
        if keyIndex == 0
          outKeys{end + 1} = currentKey;
          keyIndex = numel(outKeys);
        end
        outValues{keyIndex} = convValues{i};
      end
      outHashes = dictionary.computeKeyHashes(obj, outKeys);
    end

    function value = valueAt(obj, idx)
      if ~isempty(obj.allValues) && idx >= 1 && idx <= numel(obj.allValues)
        value = dictionary.normalizeStoredValue(obj, obj.allValues{idx});
        return
      end
      key = dictionary.normalizeStoredKey(obj, obj.allKeys{idx});
      h = dec2hex(keyHash(key));
      value = dictionary.normalizeStoredValue(obj, obj.map.(h));
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
      % Python dictionary keys are unique, so no de-duplication is needed.
      % Keys, values and hashes are built in separate loops (keys/values are
      % cells, hashes are numeric): growing a cell and a numeric array in the
      % same loop is quadratic in Nelson, which made this conversion O(N^2).
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
      obj.allKeys = allKeys;
      obj.allValues = allValues;
      obj.keyHashes = dictionary.computeKeyHashes(obj, allKeys);
    end
  end
end
%=============================================================================
