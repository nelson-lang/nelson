%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A class whose name collides with an external method of ClassdefConvSource.
% Target(sourceObj) must dispatch to the source's external method (converter),
% not to this constructor.
classdef ClassdefConvTarget
  properties
    w = 0
  end
  methods
    function obj = ClassdefConvTarget(x)
      if nargin > 0
        obj.w = x;
      end
    end
  end
end
%=============================================================================
