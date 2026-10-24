%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefInheritedIndexBase
  properties (Access = protected)
    prefList
  end
  methods
    function p = ClassdefInheritedIndexBase()
      p.prefList = struct('vectorize', true, 'maxLength', 65);
    end
    function out = subsref(p, ind)
      if strcmp(ind(1).type, '.') && isfield(p.prefList, ind(1).subs)
        out = p.prefList.(ind(1).subs);
      else
        out = builtin('subsref', p, ind);
      end
    end
    function p = subsasgn(p, ind, val)
      if strcmp(ind(1).type, '.') && isfield(p.prefList, ind(1).subs)
        p.prefList.(ind(1).subs) = val;
      else
        p = builtin('subsasgn', p, ind, val);
      end
    end
  end
end
%=============================================================================
