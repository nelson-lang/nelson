%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function obj = subsasgn(obj, s, val)
  
  if (numel (s) > 1)
    error(_('Only one level of indexing is supported.'));
  end

  switch (s(1).type)
    case '.'
      st = struct(obj);
      st.(s.subs) = val;
      obj = class(st, 'dictionary');
      return
    case '()'
      if (iscell(s(1).subs) && (numel(s(1).subs) > 1))
        error('Nelson:dictionary:TooManyInputsToParenAssign', _('Too many input arguments. To insert multiple entries, utilize an array of keys instead.'));
      end
      key = s(1).subs{1};
      if ischar(key)
        key = string (key);
      end
      if ischar(val)
        val = string(val);
      end
      if (~isConfigured(obj))
        obj.keyType = class (key);
        obj.valueType = class (val);
      end
      if (numel(key) ~= numel(val) && numel(val) ~= 1 && numel(val) ~= 0)
        error(_('Number of keys and values are not compatible'));
      end
      for k = 1:numel(key)
        currentKey = key(k);
        if (~isa(currentKey, obj.keyType))
          currentKey = convertDataType (obj.keyType, currentKey);
        end
        h = dec2hex (keyHash (currentKey));
        if (isa(val, 'double') && isequal(size (val), [0 0]))
          if isfield(obj.map, h)
            obj.map = rmfield (obj.map, h);
            obj.allKeys(cellfun (@(x) isequal (x, currentKey), obj.allKeys)) = [];
          end
          continue;
        end
        if (~isfield(obj.map, h))
          obj.allKeys{end + 1} = currentKey;
        end
        if (numel(val) == 1)
          if isa(val, obj.valueType)
            obj.map.(h) = val;
          else
            obj.map.(h) = convertDataType(obj.valueType, val);
          end
        else
          if isa(val, obj.valueType)
            obj.map.(h) = val(k);
          else
            obj.map.(h) = convertDataType(obj.valueType, val(k));
          end
        end
      end
    otherwise
      error(_("Only '()' indexing is supported for assigning values."));
  end
end
%=============================================================================
