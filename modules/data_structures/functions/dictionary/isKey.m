%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = isKey (obj, key)
  if (~isConfigured(obj))
    error (_('Unable to perform a dictionary lookup when the key and value types are not set.'));
  end
  if ischar(key)
    key = string(key);
  end  
  tf = zeros(size(key), 'like', false);
  for k = 1:numel (key)
    currentKey = key(k);
    h = dec2hex(keyHash(currentKey));
    tf(k) = isfield (obj.map, h);
  end
end
%=============================================================================
