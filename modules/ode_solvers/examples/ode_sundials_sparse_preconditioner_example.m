%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Sparse stiff system with an optional SUNDIALS preconditioner path.
%
% The portable path uses ode15s with a sparse Jacobian pattern. When Nelson is
% running with the optional backend enabled, the script also demonstrates the
% SUNDIALS CVODES sparse iterative path and plots selected components.
%=============================================================================

systemSize = 2001;
decayRates = (1:systemSize).';
initialState = ones(systemSize, 1);
finalTime = 1e-4;
sparseJacobian = -spdiags(decayRates, 0, systemSize, systemSize);

if canUseNelsonSundials()
  solverOptions = nelson.ode.options.CVODESStiff(...
    'LinearSolver', 'spgmr', ...
    'Preconditioner', 'ilu0');
  problem = ode(...
    'ODEFcn', @(t, y) - decayRates .* y, ...
    'InitialValue', initialState, ...
    'RelativeTolerance', 1e-5, ...
    'AbsoluteTolerance', 1e-7, ...
    'Jacobian', odeJacobian(sparseJacobian, 'Pattern', spones(sparseJacobian)), ...
    'SolverOptions', solverOptions);
  result = solve(problem, 0, finalTime);
  time = result.Time(:).';
  values = result.Solution;
  selectedSolver = result.RawSolution.solver;
  linearSolver = result.RawSolution.stats.linearSolver;
  preconditioner = result.RawSolution.stats.preconditioner;
else
  options = odeset(...
    'RelTol', 1e-5, ...
    'AbsTol', 1e-7, ...
    'JPattern', spones(sparseJacobian), ...
    'Jacobian', sparseJacobian);
  solution = ode15s(@(t, y) - decayRates .* y, [0 finalTime], initialState, options);
  time = solution.x;
  values = solution.y;
  selectedSolver = 'ode15s';
  linearSolver = 'default';
  preconditioner = 'not-used';
end

expectedFirst = exp(-finalTime);
computedFirst = values(1, length(time));
firstError = abs(computedFirst - expectedFirst);

disp('Sparse stiff system');
disp(['  selected solver:       ', selectedSolver]);
disp(['  linear solver:         ', linearSolver]);
disp(['  preconditioner:        ', preconditioner]);
disp(['  first component error: ', num2str(firstError)]);

if firstError >= 1e-5
  error('Sparse stiff solution is outside tolerance.');
end

if exist('odeSundialsSparsePlotEnabled', 'var') == 0
  odeSundialsSparsePlotEnabled = true;
end

if odeSundialsSparsePlotEnabled && exist('figure') ~= 0
  if exist('odeSundialsSparsePlotVisible', 'var') == 0
    odeSundialsSparsePlotVisible = 'on';
  end
  selectedRows = [1, fix(systemSize / 2), systemSize];
  figure('Visible', odeSundialsSparsePlotVisible);
  plot(time, values(selectedRows, :).');
  grid on
  xlabel('time');
  ylabel('state value');
  title('Sparse stiff system: selected components');
end

%=============================================================================
% LOCAL FUNCTIONS
%=============================================================================

function tf = canUseNelsonSundials()
  tf = false;
  if exist('__ode_sundials_capabilities__') == 0
    return
  end
  runtimeMode = upper(getenv('NELSON_SUNDIALS_RUNTIME'));
  if strcmp(runtimeMode, 'OFF') || strcmp(runtimeMode, '0') || strcmp(runtimeMode, 'FALSE')
    return
  end
  capabilities = feval('__ode_sundials_capabilities__');
  tf = capabilities.available && capabilities.spgmr;
end
%=============================================================================
