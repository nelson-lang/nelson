%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Concrete subclass. The abstract instance method is supplied by an external
% file instValue.m that is NOT declared here (the key case: an undeclared
% external method still satisfies the abstract requirement). The abstract static
% method is declared as a signature and implemented in the external staticValue.m.
classdef ClassdefAbstractExternalImpl < ClassdefAbstractExternalBase
  methods
    function obj = ClassdefAbstractExternalImpl()
    end
  end
  methods (Static = true)
    out = staticValue(x)
  end
end
%=============================================================================
