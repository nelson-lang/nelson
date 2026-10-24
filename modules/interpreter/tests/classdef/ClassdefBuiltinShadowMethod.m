%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefBuiltinShadowMethod
  methods (Static)
    function h = isa(r, dtest)
      % an unqualified isa(...) with a non-class argument must call the
      % builtin, not this method (otherwise it recurses forever)
      h = ~isa(r, 'double');
    end
    function n = numel(a)
      % likewise for a numel-shadowing method
      n = numel(a) + 1000;
    end
  end
end
%=============================================================================
