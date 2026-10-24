%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefSplit
  properties
    Value = 11
  end

  properties (Access = private)
    HiddenValue = 4
  end

  properties (Access = protected)
    ProtectedValue = 8
  end

  methods
    r = callPrivateExternal(obj)
  end

  methods (Static)
    r = staticExternal()
  end

  methods (Access = private)
    r = privateExternal(obj)
  end

  methods (Access = protected)
    r = protectedExternal(obj)
  end
end
