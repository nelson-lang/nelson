%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
n = 100000;
x1 = randn(n, 1);
x2 = randn(n, 1) + 0.01;
[h, p, ks2stat] = kstest2(x1, x2);
asserts.isequal(size(h), [1 1]);
asserts.isequal(size(p), [1 1]);
asserts.isequal(size(ks2stat), [1 1]);
times = zeros(1, 5);
for k = 1:5
  tic();
  [h, p, ks2stat] = kstest2(x1, x2);
  times(k) = toc();
  asserts.isequal(size(h), [1 1]);
  asserts.isequal(size(p), [1 1]);
  asserts.isequal(size(ks2stat), [1 1]);
end
disp(['kstest2 observations: ', int2str(n), ' + ', int2str(n)]);
disp(['kstest2 best time: ', num2str(min(times)), ' s']);
disp(['kstest2 median time: ', num2str(median(times)), ' s']);
disp(['kstest2 mean time: ', num2str(mean(times)), ' s']);
%=============================================================================
