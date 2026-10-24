%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefMigImmutable
  properties (SetAccess = immutable)
    x
    y
  end
  methods
    function obj = ClassdefMigImmutable(a, b)
      if nargin == 2
        obj.x = a;
        obj.y = b;
      end
    end
  end
end
