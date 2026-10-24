%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function v = values(varargin)
  narginchk(1, 2)
  obj = varargin{1};
  if (~isConfigured(obj))
    error('Nelson:dictionary:unablePerformADictionaryLookup3', 'Unable to perform a dictionary lookup when the key and value types are not set.');
  end

  if (nargin < 2)
    fmt = 'uniform';
  else
    fmt = varargin{2};
    if (~ischar(fmt) && ~isstring(fmt))
      error('Nelson:dictionary:invalidFormatOption', 'format option has to be a string.');
    end
  end
  if (startsWith('cell', fmt, 'IgnoreCase', true))
    v = obj.allValues(:);
    for idx = 1:numel(v)
      if strcmp(obj.valueType, 'cell') && iscell(v{idx}) && isscalar(v{idx})
        v{idx} = v{idx}{1};
      else
        v{idx} = dictionary.normalizeStoredValue(obj, v{idx});
      end
    end
  elseif (startsWith('uniform', fmt, 'IgnoreCase', true))
    v = values(obj, 'cell');
    if strcmp(obj.valueType, 'cell')
      return
    end
    try
      v = [v{:}].';
    catch
      error('Nelson:dictionary:cannotConcatenate', 'Cannot concatenate %s. Use cell option.', 'values');
    end
  else
    error('Nelson:dictionary:formatOptionCanOnlyCell', 'format option can only be cell.');
  end
end
%=============================================================================
