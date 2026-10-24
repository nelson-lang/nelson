%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef SolverOptions
  properties (Dependent)
    SolverName
  end

  properties (Hidden)
    SolverNameValue = 'default'
    Store = struct()
  end

  methods
    function obj = SolverOptions(solver, varargin)
      obj.SolverNameValue = lower(char(solver));
      obj.Store = defaultOptions(obj.SolverNameValue);
      if nargin == 2 && (isstruct(varargin{1}) || isobject(varargin{1}))
        obj.Store = mergeOptions(obj.SolverNameValue, varargin{1});
        return
      end
      if mod(length(varargin), 2) ~= 0
        error('Nelson:optimization:nameValueExpected', 'Name-value arguments expected.');
      end
      updates = struct();
      for k = 1:2:length(varargin)
        name = canonicalName(varargin{k});
        updates.(name) = varargin{k + 1};
      end
      obj.Store = mergeOptions(obj.SolverNameValue, updates);
    end

    function value = get.SolverName(obj)
      value = obj.SolverNameValue;
    end

    function st = struct(varargin)
      % struct(obj): unset algorithm-dependent defaults stay empty in Store so
      % that a later Algorithm change re-resolves them; readers always see
      % resolved values. With field name and value pairs, an options object
      % among the values is stored as a field like any other value.
      if nargin ~= 1
        st = builtin('struct', varargin{:});
        return
      end
      obj = varargin{1};
      st = resolveAlgorithmDefaults(obj.SolverNameValue, obj.Store);
    end

    function names = fieldnames(obj)
      names = fieldnames(obj.Store);
    end

    function value = subsref(obj, s)
      if strcmp(s(1).type, '.') && isfield(obj.Store, s(1).subs)
        resolved = resolveAlgorithmDefaults(obj.SolverNameValue, obj.Store);
        value = resolved.(s(1).subs);
        if length(s) > 1
          value = builtin('subsref', value, s(2:end));
        end
      else
        value = builtin('subsref', obj, s);
      end
    end

    function obj = subsasgn(obj, s, value)
      if strcmp(s(1).type, '.')
        updates = struct();
        name = canonicalName(s(1).subs);
        updates.(name) = value;
        obj.Store = mergeOptions(obj.SolverNameValue, obj.Store, updates);
      else
        obj = builtin('subsasgn', obj, s, value);
      end
    end
  end
end
%=============================================================================
