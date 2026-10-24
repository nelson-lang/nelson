%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef MemoizedFunction < handle
  properties
    Function
    Enabled = true
  end
  properties (Access = private)
    Cache
  end
  methods
    function obj = MemoizedFunction(fh)
      obj.Function = fh;
      obj.Cache = containers.Map('KeyType', 'char', 'ValueType', 'any');
    end
    %=========================================================================
    function varargout = subsref(obj, s)
      if strcmp(s(1).type, '()')
        % read properties into locals first: this avoids re-entering subsref
        % (and mis-propagating nargout) when the call requests several outputs
        fh = obj.Function;
        enabled = obj.Enabled;
        c = obj.Cache;
        args = s(1).subs;
        no = max(nargout, 1);
        if ~enabled
          [varargout{1:no}] = feval(fh, args{:});
          return
        end
        key = '';
        cacheable = true;
        try
          key = [int2str(no), '#', MemoizedFunction.serializeArgs(args)];
        catch
          cacheable = false;
        end
        if cacheable && isKey(c, key)
          varargout = c(key);
          return
        end
        out = cell(1, no);
        [out{1:no}] = feval(fh, args{:});
        if cacheable
          c(key) = out;
        end
        varargout = out;
        return
      end
      [varargout{1:nargout}] = builtin('subsref', obj, s);
    end
    %=========================================================================
    function clearCache(obj)
      if ~isempty(keys(obj.Cache))
        remove(obj.Cache, keys(obj.Cache));
      end
    end
  end
  %===========================================================================
  methods (Static, Access = private)
    function s = serializeArgs(args)
      if isscalar(args)
        % strjoin of a single part is that part
        s = MemoizedFunction.serialize(args{1});
        return
      end
      parts = cell(1, numel(args));
      for i = 1:numel(args)
        parts{i} = MemoizedFunction.serialize(args{i});
      end
      s = strjoin(parts, '||');
    end
    %=========================================================================
    function s = serialize(v)
      cls = class(v);
      if isnumeric(v) || islogical(v) || ischar(v)
        s = [cls, ':', mat2str(v)];
      elseif isstring(v)
        s = ['string:', char(strjoin(v(:).', char(1)))];
      elseif iscell(v)
        inner = cell(1, numel(v));
        for i = 1:numel(v)
          inner{i} = MemoizedFunction.serialize(v{i});
        end
        s = ['cell:', strjoin(inner, ',')];
      else
        error('Nelson:memoize:unsupportedInput', 'unsupported input type for memoization.');
      end
    end
  end
end
%=============================================================================
