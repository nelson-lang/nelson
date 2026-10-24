%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Target of a named function handle to a static method: @Class.staticMethod.
classdef ClassdefStaticHandleTarget
  methods (Static = true)
    function r = scale(x)
      r = x * 10;
    end
  end
end
%=============================================================================
