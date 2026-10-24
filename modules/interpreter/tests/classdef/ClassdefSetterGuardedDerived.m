%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Value subclass with no own properties: the inherited property default must be
% applied without invoking the inherited set method during construction.
classdef ClassdefSetterGuardedDerived < ClassdefSetterGuardedBase
  methods
    function q = ClassdefSetterGuardedDerived(value)
      if nargin == 0
        value = 1;
      end
      q = q@ClassdefSetterGuardedBase(value);
    end
  end
end
%=============================================================================
