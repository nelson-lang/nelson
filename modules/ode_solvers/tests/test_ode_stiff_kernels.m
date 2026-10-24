%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
f = @(t, y) - 300 * (y - cos(t)) - sin(t);
options = odeset('Jacobian', -300, 'RelTol', 1e-4, 'AbsTol', 1e-7, ...
  'InitialStep', 1e-4, 'MaxStep', 0.005);
solvers = {@ode15s, @ode23s, @ode23t, @ode23tb};
solutions = cell(size(solvers));
for k = 1:length(solvers)
  solver = solvers{k};
  sol = solver(f, [0 0.01 0.02], 1, options);
  solutions{k} = sol;
  err = max(abs(sol.y(:) - cos(sol.x(:))));
  asserts.istrue(err < 2e-3);
  if strcmp(sol.solver, 'ode15s')
    asserts.istrue(sol.stats.nfevals >= sol.stats.nsteps);
  elseif strcmp(sol.solver, 'ode23s')
    % Modified Rosenbrock (2,3): three stage evaluations plus one
    % finite-difference time derivative per step.
    asserts.istrue(sol.stats.nfevals >= 4 * sol.stats.nsteps);
    asserts.istrue(err < 1e-5);
  elseif strcmp(sol.solver, 'ode23t')
    asserts.istrue(err < 1e-8);
  elseif strcmp(sol.solver, 'ode23tb')
    asserts.istrue(sol.stats.nfevals >= 3 * sol.stats.nsteps);
    asserts.istrue(err < 1e-8);
  else
    asserts.istrue(sol.stats.nfevals >= 2 * sol.stats.nsteps);
  end
end
sol15s = solutions{1};
sol23s = solutions{2};
sol23t = solutions{3};
sol23tb = solutions{4};
last15s = sol15s.y(1, size(sol15s.y, 2));
last23s = sol23s.y(1, size(sol23s.y, 2));
last23t = sol23t.y(1, size(sol23t.y, 2));
last23tb = sol23tb.y(1, size(sol23tb.y, 2));
asserts.istrue(abs(last15s - last23s) > eps);
asserts.istrue(abs(last23t - last23s) > eps);
asserts.istrue(abs(last23tb - last23s) > eps);
%=============================================================================
linearOptions = odeset('Jacobian', -1, 'RelTol', 1e-4, 'AbsTol', 1e-7, ...
  'InitialStep', 1e-3, 'MaxStep', 0.02);
linear23s = ode23s(@(t, y) 1, [0 0.5], 0, linearOptions);
linear23t = ode23t(@(t, y) 1, [0 0.5], 0, linearOptions);
linear23tb = ode23tb(@(t, y) 1, [0 0.5], 0, linearOptions);
asserts.istrue(abs(linear23s.y(length(linear23s.y)) - 0.5) < 5e-5);
asserts.istrue(abs(linear23t.y(length(linear23t.y)) - 0.5) < 5e-5);
asserts.istrue(abs(linear23tb.y(length(linear23tb.y)) - 0.5) < 5e-4);
%=============================================================================
clear global ODE_COUNTED_JACOBIAN_CALLS;
global ODE_COUNTED_JACOBIAN_CALLS;
ODE_COUNTED_JACOBIAN_CALLS = 0;
options = odeset('Jacobian', @odeCountedJacobian, 'JConstant', 'off', ...
  'RelTol', 1e-3, 'AbsTol', 1e-6, 'InitialStep', 1e-4, 'MaxStep', 1e-4);
ode15s(@(t, y) - 100 * y, [0 1e-4], 1, options);
callsWithoutConstant = ODE_COUNTED_JACOBIAN_CALLS;
ODE_COUNTED_JACOBIAN_CALLS = 0;
options = odeset(options, 'JConstant', 'on');
ode15s(@(t, y) - 100 * y, [0 1e-4], 1, options);
callsWithConstant = ODE_COUNTED_JACOBIAN_CALLS;
asserts.istrue(callsWithConstant < callsWithoutConstant);
%=============================================================================
parameterOptions = odeset('Mass', @(t, y, rate, massScale) massScale, ...
  'Jacobian', @(t, y, rate, massScale) - rate, ...
  'RelTol', 1e-5, 'AbsTol', 1e-8, 'InitialStep', 1e-4, 'MaxStep', 0.002);
parameterOptions.Parameters = {100, 2};
parameterSol = ode15s(@(t, y, rate, massScale) - rate * y, [0 0.01], 1, parameterOptions);
asserts.istrue(abs(parameterSol.y(1, size(parameterSol.y, 2)) - exp(-0.5)) < 5e-3);
%=============================================================================
orderOptions = odeset('Jacobian', -20, 'RelTol', 1e-4, 'AbsTol', 1e-7, ...
  'InitialStep', 1e-3, 'MaxStep', 0.01);
defaultOrderSol = ode15s(@(t, y) - 20 * y, [0 0.1], 1, orderOptions);
firstOrderSol = ode15s(@(t, y) - 20 * y, [0 0.1], 1, odeset(orderOptions, 'MaxOrder', 1));
defaultOrderEnd = defaultOrderSol.y(1, size(defaultOrderSol.y, 2));
firstOrderEnd = firstOrderSol.y(1, size(firstOrderSol.y, 2));
asserts.istrue(abs(defaultOrderEnd - exp(-2)) < 5e-3);
asserts.istrue(abs(firstOrderEnd - exp(-2)) < 5e-3);
asserts.istrue(abs(defaultOrderEnd - firstOrderEnd) > eps);
%=============================================================================
