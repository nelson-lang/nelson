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
options = odeset('RelTol', 1e-6, 'AbsTol', 1e-9, 'MaxStep', 0.05);
sol = ode45(@(t, y)[y(2); -y(1)], [0 1], [0; 1], options);
points = linspace(0, 1, 120);
fprintf('bench_ode_deval\n');
tic();
values = deval(sol, points);
elapsed = toc();
err = max(abs(values(1, :) - sin(points)));
fprintf('  deval: %.6f s, points=%d, err=%g\n', elapsed, length(points), err);
tic();
macroValues = zeros(size(sol.y, 1), length(points));
for k = 1:length(points)
  macroValues(:, k) = odeBenchLinearInterpolate(sol.x, sol.y, points(k), sol.extdata.slopes);
end
macroElapsed = toc();
macroErr = max(abs(macroValues(1, :) - sin(points)));
fprintf('  macro loop: %.6f s, points=%d, err=%g\n', macroElapsed, length(points), macroErr);
fprintf('  ratio macro/deval = %.2f\n', macroElapsed / max(elapsed, eps));
%=============================================================================
