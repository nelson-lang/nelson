%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Fixture: subsref('()') returns as many values as requested (nargout).
classdef ClassdefParenMultiOutput
  properties
    data
  end
  methods
    function obj = ClassdefParenMultiOutput(d)
      obj.data = d;
    end
    function varargout = subsref(obj, s)
      if strcmp(s(1).type, '()')
        idx = s(1).subs{1};
        n = max(nargout, 1);
        for k = 1:n
          varargout{k} = obj.data(idx(min(k, numel(idx))));
        end
        return
      end
      [varargout{1:nargout}] = builtin('subsref', obj, s);
    end
  end
end
%=============================================================================
