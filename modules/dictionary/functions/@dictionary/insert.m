%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
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
    error(_('Wrong number of input arguments.'));
  end
  obj = varargin{1};
  key = varargin{2};
  value = varargin{3};
  overwrite = true;
  if nargin == 5
    overwriteArgument = varargin{4};
    if (~strcmp(overwriteArgument, 'Overwrite'))
      error(_('Parameter name must be ''Overwrite''.'));
    end
    overwrite = varargin{5};
    mustBeLogicalScalar(overwrite, 5);
  end
  if ~isConfigured(obj)
    varargout{1} = dictionary(key, value);
    return
  end
  
  if overwrite
    for k = key
      obj(key) = value;
    end
  else
    for k = 1:numel(key)
      currentKey = key(k);
      if (~isa(currentKey, obj.keyType))
        currentKey = convertDataType (obj.keyType, currentKey);
      end
      h = dec2hex (keyHash (currentKey));
      if (isa(value, 'value') && isequal(size (value), [0 0]))
        if isfield(obj.map, h)
          obj.map = rmfield (obj.map, h);
          obj.allKeys(cellfun (@(x) isequal (x, currentKey), obj.allKeys)) = [];
        end
        continue;
      end
      if (~isfield(obj.map, h))
        obj.allKeys{end + 1} = currentKey;
      else
        continue
      end
      if (numel(value) == 1)
        if isa(val, obj.valueType)
          obj.map.(h) = value;
        else
          obj.map.(h) = convertDataType(obj.valueType, value);
        end
      else
        if isa(value, obj.valueType)
          obj.map.(h) = value(k);
        else
          obj.map.(h) = convertDataType(obj.valueType, value(k));
        end
      end
    end
  end
  varargout{1} = obj;
end
%=============================================================================
