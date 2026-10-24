%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Base class providing an external overload of the diff macro function, so a
% subclass inherits it (exercises inherited macro overload dispatch).
classdef ClassdefMacroOverloadBase
  properties
    v = 0
  end
  methods
    function obj = ClassdefMacroOverloadBase(x)
      if nargin > 0
        obj.v = x;
      end
    end
  end
end
%=============================================================================
