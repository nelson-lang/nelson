%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefImmutableColorPoint < ClassdefImmutablePoint
  properties (SetAccess = immutable)
    Color
  end
  methods
    function obj = ClassdefImmutableColorPoint(x, y, c)
      obj = obj@ClassdefImmutablePoint(x, y);
      if nargin >= 3
        obj.Color = c;
      end
    end
  end
end
