%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefImmutablePoint
  properties (SetAccess = immutable)
    X
    Y = 5
  end
  properties
    Label = 'p'
  end
  methods
    function obj = ClassdefImmutablePoint(x, y)
      if nargin >= 1
        obj.X = x;
      end
      if nargin >= 2
        obj.Y = y;
      end
    end
    function obj = trySetX(obj, v)
      obj.X = v; % setting an immutable property outside construction
    end
  end
end
