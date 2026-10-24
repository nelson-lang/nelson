%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [c, ceq, gc, gceq] = optimTestFminconBadConstraintGrad(x)
  c = x(1) ^ 2 - 1;
  ceq = [];
  gc = 1;
  gceq = zeros(length(x), 0);
end
%=============================================================================
