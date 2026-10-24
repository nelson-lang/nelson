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
fprintf('bench_ode_cpp_candidates\n');
%=============================================================================
addpath([modulepath('ode_solvers', 'root'), '/tests']);
points = linspace(0, 1, 1000);
options = odeset('RelTol', 1e-7, 'AbsTol', 1e-10, 'MaxStep', 0.01);
sol = ode45(@(t, y)[y(2); -y(1)], [0 1], [0; 1], options);
tic();
nativeValues = deval(sol, points);
nativeTime = toc();
tic();
macroValues = zeros(size(sol.y, 1), length(points));
for k = 1:length(points)
  macroValues(:, k) = odeBenchLinearInterpolate(sol.x, sol.y, points(k), sol.extdata.slopes);
end
macroTime = toc();
fprintf('  dense output native: %.6f s, macro loop: %.6f s, ratio=%.2f\n', ...
  nativeTime, macroTime, macroTime / max(nativeTime, eps));
asserts.istrue(max(abs(nativeValues(:) - macroValues(:))) < 1e-12);
%=============================================================================
n = 8;
a = -diag(1:n);
y0 = ones(n, 1);
stiffOptions = odeset('RelTol', 1e-4, 'AbsTol', 1e-7, 'MaxStep', 0.004);
tic();
solFiniteDifference = ode15s(@(t, y) a * y, [0 0.03], y0, stiffOptions);
finiteDifferenceTime = toc();
tic();
solProvided = ode15s(@(t, y) a * y, [0 0.03], y0, odeset(stiffOptions, 'Jacobian', a));
providedTime = toc();
fprintf('  Jacobian finite difference: %.6f s, provided: %.6f s, ratio=%.2f\n', ...
  finiteDifferenceTime, providedTime, finiteDifferenceTime / max(providedTime, eps));
asserts.istrue(solFiniteDifference.stats.nfevals > solProvided.stats.nfevals);
%=============================================================================
plainOptions = odeset('RelTol', 1e-6, 'AbsTol', 1e-9, 'MaxStep', 0.01);
tic();
plainSol = ode45(@odeBenchProbeRhs, [0 0.2], [1; 0], plainOptions);
plainTime = toc();
tic();
eventSol = ode45(@odeBenchProbeRhs, [0 0.2], [1; 0], ...
  odeset(plainOptions, 'Events', @odeBenchProbeEvent));
eventTime = toc();
tic();
outputSol = ode45(@odeBenchProbeRhs, [0 0.2], [1; 0], ...
  odeset(plainOptions, 'OutputFcn', @odeBenchProbeOutput, 'Refine', 4));
outputTime = toc();
fprintf('  callback baseline: %.6f s, events: %.6f s, output: %.6f s\n', ...
  plainTime, eventTime, outputTime);
asserts.istrue(plainSol.stats.nsteps > 0);
asserts.istrue(eventSol.stats.nsteps > 0);
asserts.istrue(outputSol.stats.nsteps > 0);
%=============================================================================
function dydt = odeBenchProbeRhs(t, y)
  dydt = [y(2); -y(1)];
end
%=============================================================================
function [value, isterminal, direction] = odeBenchProbeEvent(t, y)
  value = y(1) - 0.5;
  isterminal = 0;
  direction = 0;
end
%=============================================================================
function stop = odeBenchProbeOutput(t, y, flag)
  stop = false;
end
%=============================================================================
