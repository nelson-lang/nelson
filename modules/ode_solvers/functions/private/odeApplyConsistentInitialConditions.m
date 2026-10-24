%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [initialValue, initialSlope, options] = ...
    odeApplyConsistentInitialConditions(solver, odefun, t0, initialValue, initialSlope, options)
  if ~(strcmp(solver, 'ode15i') || strcmp(solver, 'idas')) ...
      || ~odeShouldComputeConsistentInitialConditions(options)
    return
  end
  initialValue = initialValue(:);
  if isempty(initialSlope)
    initialSlope = zeros(size(initialValue));
  else
    initialSlope = initialSlope(:);
  end
  residual = @(t, y, yp) odeCallFcn(odefun, t, y, options, yp);
  fixedValue = true(size(initialValue));
  fixedSlope = false(size(initialSlope));
  [initialValue, initialSlope] = decic(residual, t0, initialValue, fixedValue, ...
    initialSlope, fixedSlope, options);
  odeCheckConsistentInitialResidual(residual, t0, initialValue, initialSlope, options);
  options.InitialSlope = initialSlope;
end
%=============================================================================
function tf = odeShouldComputeConsistentInitialConditions(options)
  tf = isfield(options, 'ComputeConsistentInitialConditions') ...
    && logical(options.ComputeConsistentInitialConditions);
end
%=============================================================================
function odeCheckConsistentInitialResidual(residual, t0, initialValue, initialSlope, options)
  value = residual(t0, initialValue, initialSlope);
  value = value(:);
  if any(~isfinite(value))
    error('Nelson:ode_solvers:consistentInitialConditionsProducedNonFinite', 'Consistent initial conditions produced a non-finite residual.');
  end
  absTolerance = max(options.AbsTol(:));
  relTolerance = max(options.RelTol(:));
  scale = max(absTolerance, relTolerance * max(1, max(abs(initialValue(:)))));
  if norm(value, inf) > 1000 * scale
    error('Nelson:ode_solvers:consistentInitialConditionsResidualTooLarge', 'Consistent initial conditions residual is too large.');
  end
end
%=============================================================================
