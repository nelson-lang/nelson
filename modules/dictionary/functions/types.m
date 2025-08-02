%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = types(varargin)
  narginchk(1, 1);
  nargoutchk(0, 2);
  obj = varargin{1};
  if (~isConfigured (obj))
    keyType = string(NaN);
    valueType = string(NaN);
  else
    keyType = convertCharsToStrings(obj.keyType);
    valueType = convertCharsToStrings(obj.valueType);
  end
  varargout{1} = keyType;
  if nargout > 1
    varargout{2} = valueType;
  end
end
%=============================================================================
