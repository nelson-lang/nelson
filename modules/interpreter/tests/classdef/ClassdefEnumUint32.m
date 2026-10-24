%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefEnumUint32 < uint32
  enumeration
    Low(1)
    High(2)
  end

  methods
    function r = isHigh(obj)
      r = obj == ClassdefEnumUint32.High;
    end
  end
end
