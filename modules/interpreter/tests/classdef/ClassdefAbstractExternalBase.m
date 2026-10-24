%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Abstract base whose abstract methods are implemented by a subclass through
% external method files (not declared in the subclass classdef).
classdef ClassdefAbstractExternalBase
  methods (Abstract = true)
    out = instValue(obj);
  end
  methods (Abstract = true, Static = true)
    out = staticValue(x);
  end
end
%=============================================================================
