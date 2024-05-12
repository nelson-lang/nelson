%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function v = values (varargin)
  narginchk(1, 2)
  obj = varargin{1};
  if (~isConfigured (obj))
    error (_('Unable to perform a dictionary lookup when the key and value types are not set.'));
  end
  
  if (nargin < 2)
    fmt = 'uniform';
  else
    fmt = varargin{2};
    if (~ischar (fmt) && ~isstring (fmt))
      error(_('format option has to be a string'));
    end
  end

  if (startsWith('cell', fmt, 'IgnoreCase', true))
    v = struct2cell(obj.map);
  elseif (startsWith ('uniform', fmt, 'IgnoreCase', true))
    v = struct2cell(obj.map);
    try
      v = [v{:}].';
    catch
      error(_('cannot concatenate values. Use cell option.'));
    end
  else
    error (_('format option can only be cell.'));
  end
end
%=============================================================================
