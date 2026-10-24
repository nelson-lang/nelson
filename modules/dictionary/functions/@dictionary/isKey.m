%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = isKey(varargin)
  narginchk(2, 2);
  nargoutchk(0, 1);
  obj = varargin{1};
  key = varargin{2};
  if (~isConfigured(obj))
    error('Nelson:dictionary:unablePerformADictionaryLookup3', 'Unable to perform a dictionary lookup when the key and value types are not set.');
  end
  [keyType, valueType] = types(obj);
  key = dictionaryLookupKey(key, keyType);
  % Resolve every query key in one vectorized pass; a per-key findKeyIndex
  % loop is quadratic because each call scans all stored keys.
  idx = dictionary.findKeyIndices(obj, key);
  varargout{1} = reshape(idx > 0, size(key));
end
%=============================================================================
function key = dictionaryLookupKey(key, keyType)
  if strcmp(keyType, 'string') && ischar(key)
    key = string(key);
  end
end
%=============================================================================
