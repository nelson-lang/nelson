%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function gradient = odeProjectedAdjointGradient(sol, sensitivity, parameters, indices)
  n = size(sol.y, 1);
  p = length(indices);
  gradient = zeros(p, 1);
  if ~isempty(sensitivity.ObjectiveFcn)
    tfinal = sol.x(length(sol.x));
    yfinal = sol.y(:, length(sol.x));
    sfinal = reshape(sol.sensitivity(:, :, length(sol.x)), n, p);
    base = odeAdjointScalar(sensitivity.ObjectiveFcn, tfinal, yfinal, parameters);
    gradient = gradient + odeAdjointParameterGradient(...
      sensitivity.ObjectiveFcn, tfinal, yfinal, parameters, indices, base);
    gradient = gradient + sfinal.' * odeAdjointStateGradient(...
      sensitivity.ObjectiveFcn, tfinal, yfinal, parameters, base);
  end
  if ~isempty(sensitivity.QuadratureFcn)
    gradient = gradient + odeAdjointQuadratureProjectedGradient(...
      sol, sensitivity.QuadratureFcn, parameters, indices);
  end
end
%=============================================================================
function gradient = odeAdjointQuadratureProjectedGradient(sol, fcn, parameters, indices)
  p = length(indices);
  integrand = zeros(p, length(sol.x));
  for k = 1:length(sol.x)
    y = sol.y(:, k);
    s = sol.sensitivity(:, :, k);
    base = odeAdjointScalar(fcn, sol.x(k), y, parameters);
    integrand(:, k) = odeAdjointParameterGradient(fcn, sol.x(k), y, parameters, indices, base) ...
      + s.' * odeAdjointStateGradient(fcn, sol.x(k), y, parameters, base);
  end
  gradient = zeros(p, 1);
  for k = 1:length(sol.x) - 1
    dt = sol.x(k + 1) - sol.x(k);
    gradient = gradient + 0.5 * dt * (integrand(:, k) + integrand(:, k + 1));
  end
end
%=============================================================================
function value = odeAdjointScalar(fcn, t, y, parameters)
  value = feval(fcn, t, y, parameters);
  if ~isnumeric(value) || ~isreal(value) || ~isscalar(value)
    error('Nelson:ode_solvers:adjointScalarFunctionReturnRealScalar', 'Adjoint scalar function must return a real scalar.');
  end
end
%=============================================================================
function gradient = odeAdjointStateGradient(fcn, t, y, parameters, base)
  gradient = zeros(length(y), 1);
  for k = 1:length(y)
    step = sqrt(eps) * max(1, abs(y(k)));
    trial = y;
    trial(k) = trial(k) + step;
    gradient(k) = (odeAdjointScalar(fcn, t, trial, parameters) - base) / step;
  end
end
%=============================================================================
function gradient = odeAdjointParameterGradient(fcn, t, y, parameters, indices, base)
  gradient = zeros(length(indices), 1);
  for k = 1:length(indices)
    idx = indices(k);
    step = sqrt(eps) * max(1, abs(parameters(idx)));
    trial = parameters;
    trial(idx) = trial(idx) + step;
    gradient(k) = (odeAdjointScalar(fcn, t, y, trial) - base) / step;
  end
end
%=============================================================================
