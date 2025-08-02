%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = isKey (varargin)
  narginchk(2, 2);
  nargoutchk(0, 1);
  obj = varargin{1};
  key = varargin{2};
  if (~isConfigured(obj))
    error (_('Unable to perform a dictionary lookup when the key and value types are not set.'));
  end
  [keyType, valueType] = types(obj);
  tf = zeros(size(key), 'like', false);
  for k = 1:numel (key)
    currentKey = convertDataType(keyType, key(k));
    h = dec2hex(keyHash(currentKey));
    tf(k) = isfield (obj.map, h);
  end
  varargout{1} = tf;
end
%=============================================================================
