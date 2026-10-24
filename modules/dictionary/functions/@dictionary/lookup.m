%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = lookup(varargin)
  % TO DO: move to C++
  narginchk(2, 4);
  nargoutchk(0, 1);
  obj = varargin{1};
  if (~isConfigured(obj))
    error('Nelson:dictionary:unablePerformADictionaryLookup3', 'Unable to perform a dictionary lookup when the key and value types are not set.');
  end
  if nargin == 3
    error('Nelson:wrongNumberOfInputs', 'Wrong number of input arguments.');
  end
  key = varargin{2};
  [keyType, valueType] = types(obj);
  key = dictionaryLookupKey(key, keyType);
  if nargin == 2
    if (~any(isKey(obj, key)))
      error('Nelson:dictionary:keyNotFound', 'Key not found in dictionary.');
    end
    varargout{1} = obj(key);
    return;
  end
  fallbackArgument = varargin{3};
  if (~strcmp(varargin{3}, 'FallbackValue'))
    error('Nelson:dictionary:invalidParameterName', 'Parameter name must be ''%s''.', 'FallbackValue');
  end
  fallbackValue = varargin{4};
  if ~isscalar(fallbackValue)
    error('Nelson:validators:mustBeScalar', '%s must be a scalar.', 'Fallback value');
  end
  if all(isKey(obj, key))
    varargout{1} = obj(key);
    return
  end
  if strcmp(valueType, 'cell')
    varargout{1} = lookupCellValues(obj, key, fallbackValue);
    return
  end
  if ~strcmp(class(fallbackValue), valueType)
    fallbackValue = feval(valueType, fallbackValue);
  end
  nbKeys = numel(key);
  present = isKey(obj, key);
  if nbKeys == 0
    V = fallbackValue([]);
  else
    V = repmat(fallbackValue, 1, nbKeys);
    for i = 1:nbKeys
      if present(i)
        V(i) = obj(key(i));
      end
    end
  end

  varargout{1} = reshape(V, size(key));
end
%=============================================================================
function key = dictionaryLookupKey(key, keyType)
  if strcmp(keyType, 'string') && ischar(key)
    key = string(key);
  end
end
%=============================================================================
function V = lookupCellValues(obj, key, fallbackValue)
  if ~iscell(fallbackValue)
    fallbackValue = {fallbackValue};
  end
  V = cell(size(key));
  for i = 1:numel(key)
    currentKey = key(i);
    if isKey(obj, currentKey)
      currentValue = obj(currentKey);
      if iscell(currentValue) && isscalar(currentValue)
        V{i} = currentValue{1};
      else
        V{i} = currentValue;
      end
    else
      V{i} = fallbackValue{1};
    end
  end
end
%=============================================================================
