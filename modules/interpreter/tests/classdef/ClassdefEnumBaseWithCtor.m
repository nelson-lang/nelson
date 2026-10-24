%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefEnumBaseWithCtor
  properties (SetAccess = immutable)
    A double
    B double
  end
  methods
    function obj = ClassdefEnumBaseWithCtor(a, b)
      if nargin > 0
        obj.A = a;
        obj.B = b;
      end
    end
  end
end
