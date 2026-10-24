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
n = 4;
d = -120 * ones(n, 1);
off = 20 * ones(n - 1, 1);
a = diag(d) + diag(off, 1) + diag(off, -1);
y0 = ones(n, 1);
rhs = @(t, y) a * y;
baseOptions = odeset('RelTol', 1e-3, 'AbsTol', 1e-6, ...
  'InitialStep', 1e-4, 'MaxStep', 0.001);
fprintf('bench_ode_stiff_jacobian\n');
tic();
solFiniteDifference = ode15s(rhs, [0 0.004], y0, baseOptions);
finiteDifferenceTime = toc();
fprintf('  finite-difference Jacobian: %.6f s, steps=%d, evals=%d\n', ...
  finiteDifferenceTime, solFiniteDifference.stats.nsteps, ...
  solFiniteDifference.stats.nfevals);
tic();
solProvided = ode15s(rhs, [0 0.004], y0, odeset(baseOptions, 'Jacobian', a));
providedTime = toc();
fprintf('  provided Jacobian: %.6f s, steps=%d, evals=%d\n', ...
  providedTime, solProvided.stats.nsteps, solProvided.stats.nfevals);
ratio = finiteDifferenceTime / max(providedTime, eps);
fprintf('  ratio finite-difference/provided = %.2f\n', ratio);
%=============================================================================
