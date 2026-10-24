%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--RELEASE ONLY-->
%=============================================================================
options = odeset('RelTol', 1e-6, 'AbsTol', 1e-9, 'MaxStep', 0.1);
rhs = @(t, y) y;
solvers = {@ode45, @ode78, @ode89, @ode113};
fprintf('bench_ode_explicit_kernels\n');
for k = 1:length(solvers)
  solver = solvers{k};
  tic();
  sol = solver(rhs, [0 0.5], 1, options);
  elapsed = toc();
  err = abs(sol.y(1, size(sol.y, 2)) - exp(0.5));
  fprintf('  %s: %.6f s, steps=%d, evals=%d, err=%g\n', ...
    sol.solver, elapsed, sol.stats.nsteps, sol.stats.nfevals, err);
end
%=============================================================================
