%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function k = keys(varargin)
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
    k = obj.allKeys(:).';
    for idx = 1:numel(k)
      k{idx} = dictionary.normalizeStoredKey(obj, k{idx});
    end
  elseif (startsWith('uniform', fmt, 'IgnoreCase', true))
    try
      k = keys(obj, 'cell');
      keyType = types(obj);
      if strcmp(keyType, 'string')
        % N-ary horzcat of the string scalars is pairwise and O(numKeys^2);
        % string() gathers the cell in one native pass. Force a column to
        % match the [k{:}].' layout.
        k = string(k);
        k = k(:);
      else
        k = [k{:}].';
      end
    catch
      error('Nelson:dictionary:cannotConcatenate', 'Cannot concatenate %s. Use cell option.', 'keys');
    end
  else
    error('Nelson:dictionary:formatOptionCanOnlyCell', 'format option can only be cell.');
  end
end
%=============================================================================
