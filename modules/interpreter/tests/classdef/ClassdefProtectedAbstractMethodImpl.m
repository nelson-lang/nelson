%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefProtectedAbstractMethodImpl < ClassdefProtectedAbstractMethod
  methods (Access = protected)
    function value = protectedValue(obj)
      value = 6;
    end
  end

  methods
    function value = readValue(obj)
      value = obj.protectedValue();
    end
  end
end
