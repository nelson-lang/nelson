%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = subsref(varargin)
  narginchk(2, 2);
  obj = varargin{1};
  s = varargin{2};
  
  switch (s(1).type)
    case '.'
      extra = {};
      if (numel (s) > 1 && strcmp (s(2).type, '()'))
        if (~isempty (s(2).subs))
          extra = s(2).subs;
        end
        s(2) = [];
      end
      switch (s(1).subs)
        case 'map'
          st = struct(obj);
          varargout{1} = st.map;
        case 'allKeys'
          st = struct(obj);
          varargout{1} = st.allKeys;
        case 'keyType'
          st = struct(obj);
          varargout{1} = st.keyType;
        case 'valueType'
          st = struct(obj);
          varargout{1} = st.valueType;
        case 'keys'
          varargout{1} = keys(obj, extra{:});
        case 'values'
          varargout{1} = values(obj, extra{:});
        case 'types'
          varargout{1} = types(obj, extra{:});
        case 'numEntries'
          varargout{1} = numEntries(obj, extra{:});
        case 'isConfigured'
          varargout{1} = isConfigured(obj, extra{:});
        case 'isKey'
          varargout{1} = isKey(obj, extra{:});
        otherwise
          msg = sprintf(_("unknown property '%s'."), s(1).subs);
          error(msg);
        end
        return
      case '()'
        if (isempty (s(1).subs))
          error ("No key specified");
        end
        if (iscell(s(1).subs) && (numel(s(1).subs) > 1))
          error('Nelson:dictionary:TooManyInputsToParenReference', _('Too many input arguments. To search for multiple entries, utilize an array of keys instead.'));
        end
        if (~isConfigured(obj))
          error ("Dictionary not configured");
        end
        key = s(1).subs{1};
        if ischar (key)
          key = string (key);
        end
        res = cell(size (key));
        for k = 1:numel (key)
          currentKey = key(k);
          if (~isa(currentKey, obj.keyType))
            currentKey = convertDataType(obj.keyType, currentKey);
          end
          h = dec2hex(keyHash(currentKey));
          if isfield(obj.map, h)
            res{k} = obj.map.(h);
          else
            error ("Unknown key");
          end
        end
        try
          res = [res{:}];
        catch
          error('Cannot concatenate values.');
        end
        res = reshape(res, size (key));
        varargout{1} = res;
      case '{}'
        if (isempty (s(1).subs))
          error ("No key specified");
        end
        if (iscell(s(1).subs) && (numel(s(1).subs) > 1))
          error('Nelson:dictionary:TooManyInputsToParenReference', _('Too many input arguments. To search for multiple entries, utilize an array of keys instead.'));
        end
        if (~isConfigured(obj))
          error ("Dictionary not configured");
        end
        if ~strcmp(obj.valueType, 'cell')
          error(_("The dictionary value type must be 'cell'."));
        end
        key = s(1).subs{1};
        if ischar (key)
          key = string (key);
        end
        res = cell(size (key));
        for k = 1:numel (key)
          currentKey = key(k);
          if (~isa(currentKey, obj.keyType))
            currentKey = convertDataType(obj.keyType, currentKey);
          end
          h = dec2hex(keyHash(currentKey));
          if isfield(obj.map, h)
            res{k} = obj.map.(h);
          else
            error ("Unknown key");
          end
        end
        try
          res = [res{:}];
        catch
          error('Cannot concatenate values.');
        end
        res = reshape(res, size (key));
        varargout{1} = res{:};
      otherwise
        msg = _("'%s' indexing is not supported.", s(1).type);
        error (msg);
      end
      if (numel (s) > 1)
        varargout{1} = subsref(res, s(2:end));
      end
    end
%=============================================================================
