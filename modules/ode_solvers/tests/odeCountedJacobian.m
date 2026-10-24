%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function jac = odeCountedJacobian(t, y)
  global ODE_COUNTED_JACOBIAN_CALLS;
  if isempty(ODE_COUNTED_JACOBIAN_CALLS)
    ODE_COUNTED_JACOBIAN_CALLS = 0;
  end
  ODE_COUNTED_JACOBIAN_CALLS = ODE_COUNTED_JACOBIAN_CALLS + 1;
  jac = -100;
end
%=============================================================================
