%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function dy = odeRhs(problem, t, y)
  if strcmp(problem.solver, 'ode15i')
    dy = odeImplicitSlope(problem, t, y);
  else
    dy = odeCallFcn(problem.odefun, t, y, problem.options);
  end
  if ~isempty(problem.options.Mass)
    mass = problem.options.Mass;
    if isa(mass, 'function_handle')
      mass = odeCallFcn(mass, t, y, problem.options);
    end
    dy = mass \ dy;
  end
  dy = dy(:);
end
%=============================================================================
