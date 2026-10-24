%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
% <--REUSE PROCESS-->
%=============================================================================
n = 1000000;
x = linspace(1, 10, n);
p = linspace(0.01, 0.99, n);
times = zeros(1, 5);
for k = 1:5
  tic();
  [m1, v1] = normstat(x, 2);
  [m2, v2] = gamstat(x, 2);
  [m3, v3] = chi2stat(x);
  [m4, v4] = tstat(x + 2);
  [m5, v5] = fstat(x, x + 5);
  [m6, v6] = betastat(x, x + 1);
  [m7, v7] = binostat(floor(x), p);
  [m8, v8] = poissstat(x);
  times(k) = toc();
  asserts.isequal(size(m1), [1 n]);
  asserts.isequal(size(v8), [1 n]);
end
disp(['distribution stat elements: ', int2str(n)]);
disp(['distribution stat best time: ', num2str(min(times)), ' s']);
disp(['distribution stat median time: ', num2str(median(times)), ' s']);
disp(['distribution stat mean time: ', num2str(mean(times)), ' s']);
%=============================================================================
