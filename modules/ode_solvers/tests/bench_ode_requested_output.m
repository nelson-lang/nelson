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
points = linspace(0, 1, 120);
options = odeset('RelTol', 1e-6, 'AbsTol', 1e-9, 'MaxStep', 0.05);
fprintf('bench_ode_requested_output\n');
tic();
[t, y] = ode45(@(t, y)[y(2); -y(1)], points, [0; 1], options);
elapsed = toc();
err = max(abs(y(:, 1) - sin(t)));
fprintf('  requested output: %.6f s, points=%d, err=%g\n', elapsed, length(points), err);
%=============================================================================
% Span scaling: the cost of a single call over 0:0.01:T must grow roughly
% linearly with T (storage growth and requested-output lookup are amortized
% O(1) per stored point, not a rescan of the whole tspan per step).
spans = [10, 40];
elapsedBySpan = zeros(1, length(spans));
for k = 1:length(spans)
  tspan = 0:0.01:spans(k);
  tic();
  [t, y] = ode45(@(t, y)[y(2); -y(1)], tspan, [0; 1], options);
  elapsedBySpan(k) = toc();
  fprintf('  span T=%d: %.6f s, points=%d\n', spans(k), elapsedBySpan(k), length(tspan));
end
fprintf('  span scaling ratio T=40/T=10: %.2f (linear ~4)\n', ...
  elapsedBySpan(2) / max(elapsedBySpan(1), eps));
%=============================================================================
