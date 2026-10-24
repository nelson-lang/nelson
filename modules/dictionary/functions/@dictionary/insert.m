%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = insert(varargin)
  % TO DO: move to C++
  nargoutchk(0, 1);
  narginchk(3, 5);
  if (nargin == 4)
    error('Nelson:wrongNumberOfInputs', 'Wrong number of input arguments.');
  end
  obj = varargin{1};
  key = varargin{2};
  value = varargin{3};
  overwrite = true;
  if nargin == 5
    overwriteArgument = varargin{4};
    if (~strcmp(overwriteArgument, 'Overwrite'))
      error('Nelson:dictionary:invalidParameterName', 'Parameter name must be ''%s''.', 'Overwrite');
    end
    overwrite = varargin{5};
    mustBeLogicalScalar(overwrite, 5);
  end
  if ~isConfigured(obj)
    varargout{1} = dictionary(key, value);
    return
  end

  if overwrite
    obj(key) = value;
  else
    % Resolve all keys in one bulk pass (linear for numeric/string keys); a
    % per-key findKeyIndex loop would be O(numel(key) * nEntries).
    missing = reshape(dictionary.findKeyIndices(obj, key) == 0, size(key));
    if ~any(missing)
      varargout{1} = obj;
      return
    end
    if numel(value) == 1
      valueToInsert = value;
    elseif iscell(value)
      valueToInsert = value(missing);
    else
      valueToInsert = value(missing);
    end
    if strcmp(obj.keyType, 'cell')
      obj(dictionarySubset(key, missing)) = valueToInsert;
    else
      obj(dictionarySubset(key, missing)) = valueToInsert;
    end
  end
  varargout{1} = obj;
end
%=============================================================================
function values = dictionarySubset(values, mask)
  if iscell(values)
    values = values(mask);
  else
    values = values(mask);
  end
end
%=============================================================================
