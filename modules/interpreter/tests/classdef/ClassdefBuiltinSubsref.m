%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% An overloaded subsref/subsasgn whose fallback branch calls
% builtin('subsref'/'subsasgn', ...) for the default structural indexing. The
% builtin must NOT re-enter this class's overloaded subsref/subsasgn (which
% would recurse). This mirrors Chebfun's chebpref/cheboppref preference objects.
classdef ClassdefBuiltinSubsref
  properties
    name
  end
  properties (Access = protected)
    prefList
  end
  methods
    function p = ClassdefBuiltinSubsref()
      p.name = 'default';
      p.prefList = struct('vectorize', true, 'maxLength', 65);
    end
    function out = subsref(p, ind)
      if strcmp(ind(1).type, '.') && isfield(p.prefList, ind(1).subs)
        out = p.prefList.(ind(1).subs);
      else
        % default property lookup (name, or an error for a missing property)
        out = builtin('subsref', p, ind);
      end
    end
    function p = subsasgn(p, ind, val)
      if strcmp(ind(1).type, '.') && isfield(p.prefList, ind(1).subs)
        p.prefList.(ind(1).subs) = val;
      else
        % default property assignment (name, or an error for a missing property)
        p = builtin('subsasgn', p, ind, val);
      end
    end
  end
end
%=============================================================================
