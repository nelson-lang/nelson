%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function k = keys(varargin)
  obj = varargin{1};
  if (~isConfigured (obj))
    error (_('Unable to perform a dictionary lookup when the key and value types are not set.'));
  end
  
  if (nargin < 2)
    fmt = 'uniform';
  else
    fmt = varargin{2};
    if (~ischar (fmt) && ~isstring (fmt))
      error(_('format option has to be a string.'));
    end
  end
  
  if (startsWith ('cell', fmt, 'IgnoreCase', true))
    k = obj.allKeys(:).';
  elseif (startsWith ('uniform', fmt, 'IgnoreCase', true))
    try
      k = obj.allKeys;
      k = [k{:}].';
    catch
      error (_('Cannot concatenate keys. Use cell option.'));
    end
  else
    error (_('format option can only be cell'));
  end
end
%=============================================================================
