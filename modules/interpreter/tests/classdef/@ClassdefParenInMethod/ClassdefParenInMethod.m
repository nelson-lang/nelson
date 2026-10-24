%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A class with an overloaded subsref (in an external file) that indexes the
% object with () inside the method: this must use the built-in subset instead of
% re-entering subsref.
classdef ClassdefParenInMethod
  properties
    v = 42
  end
  methods
    function obj = ClassdefParenInMethod(x)
      if nargin == 1
        obj.v = x;
      end
    end
    varargout = subsref(f, index)
  end
end
