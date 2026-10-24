%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [y0new, yp0new, resnrm] = decic(odefun, t0, y0, fixedY0, yp0, fixedYp0, options)
  narginchk(6, 7);
  nargoutchk(0, 3);
  if nargin < 7
    options = odeset();
  end
  if ~isa(odefun, 'function_handle')
    error('Nelson:ode_solvers:functionHandleExpected', '%s must be a function handle.', 'Value');
  end
  if ~isnumeric(t0) || ~isscalar(t0) || ~isfinite(t0)
    error('Nelson:ode_solvers:initialTimeFiniteScalar', 'Initial time must be a finite scalar.');
  end
  yShape = size(y0);
  ypShape = size(yp0);
  y = decicVector(y0, 'y0');
  yp = decicVector(yp0, 'yp0');
  if length(y) ~= length(yp)
    error('Nelson:ode_solvers:initialValueInitialSlopeSameLength', 'Initial value and initial slope must have the same length.');
  end
  fixedY = decicFixedMask(fixedY0, length(y), 'fixed_y0');
  fixedYp = decicFixedMask(fixedYp0, length(y), 'fixed_yp0');
  free = find(~[fixedY; fixedYp]);
  if isempty(free)
    error('Nelson:ode_solvers:leastOneInitialComponentFree', 'At least one initial component must be free.');
  end
  reltol = odeget(options, 'RelTol', 1e-3);
  abstol = odeget(options, 'AbsTol', 1e-6);
  tol = max(reltol, max(abstol(:)));
  [y, yp, resnrm] = decicNewton(odefun, t0, y, fixedY, yp, fixedYp, free, tol);
  y0new = reshape(y, yShape);
  yp0new = reshape(yp, ypShape);
end
%=============================================================================
function value = decicVector(value, name)
  if ~isnumeric(value) || ~isvector(value) || any(~isfinite(value(:)))
    error('Nelson:ode_solvers:mustBeFiniteVector', '%s must be a finite numeric vector.', name);
  end
  value = value(:);
end
%=============================================================================
function mask = decicFixedMask(value, n, name)
  if isempty(value)
    mask = false(n, 1);
    return
  end
  if ~(isnumeric(value) || islogical(value)) || ~isvector(value) || length(value(:)) ~= n
    error('Nelson:ode_solvers:sizeMismatch:valueEmptyAVectorLengthY0', '%s must be empty or a vector with the same length as y0.', name);
  end
  mask = logical(value(:));
end
%=============================================================================
function [y, yp, resnrm] = decicNewton(odefun, t0, y, fixedY, yp, fixedYp, free, tol)
  z = [y; yp];
  fixed = [fixedY; fixedYp];
  for k = 1:12
    y = z(1:length(y));
    yp = z(length(y) + 1:length(z));
    residual = feval(odefun, t0, y, yp);
    residual = residual(:);
    resnrm = norm(residual, inf);
    if resnrm <= tol
      return
    end
    jac = decicFiniteDifferenceJacobian(odefun, t0, z, residual, free);
    deltaFree = jac \ residual;
    z(free) = z(free) - deltaFree;
    current = [y; yp];
    z(fixed) = current(fixed);
    if norm(deltaFree, inf) <= tol * (1 + norm(z(free), inf))
      y = z(1:length(y));
      yp = z(length(y) + 1:length(z));
      residual = feval(odefun, t0, y, yp);
      resnrm = norm(residual(:), inf);
      return
    end
  end
  error('Nelson:ode_solvers:unableComputeConsistentInitialConditions', 'Unable to compute consistent initial conditions.');
end
%=============================================================================
function jac = decicFiniteDifferenceJacobian(odefun, t0, z, residual, free)
  n = length(z) / 2;
  jac = zeros(length(residual), length(free));
  for k = 1:length(free)
    idx = free(k);
    step = sqrt(eps) * max(abs(z(idx)), 1);
    trial = z;
    trial(idx) = trial(idx) + step;
    ftrial = feval(odefun, t0, trial(1:n), trial(n + 1:length(trial)));
    jac(:, k) = (ftrial(:) - residual) / step;
  end
end
%=============================================================================
