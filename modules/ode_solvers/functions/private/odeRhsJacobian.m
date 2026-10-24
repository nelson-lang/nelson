%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [jac, evals] = odeRhsJacobian(problem, t, y, f0)
  evals = 0;
  jac = problem.options.Jacobian;
  if isempty(jac)
    [jac, evals] = odeFiniteDifferenceRhsJacobian(problem, t, y, f0);
    return
  end
  if isa(jac, 'function_handle')
    jac = odeCallFcn(jac, t, y, problem.options);
    evals = 1;
  end
  if ~isempty(problem.options.Mass)
    mass = problem.options.Mass;
    if isa(mass, 'function_handle')
      mass = odeCallFcn(mass, t, y, problem.options);
      evals = evals + 1;
    end
    jac = mass \ jac;
  end
end
%=============================================================================
