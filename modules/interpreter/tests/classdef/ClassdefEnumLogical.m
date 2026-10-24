%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefEnumLogical < logical
  enumeration
    Off(false)
    On(true)
  end

  methods
    function r = isOn(obj)
      r = obj == ClassdefEnumLogical.On;
    end
  end
end
%=============================================================================
