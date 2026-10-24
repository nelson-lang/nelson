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
x = randn(n, 1);
[h, p, ksstat] = kstest(x);
asserts.isequal(size(h), [1 1]);
asserts.isequal(size(p), [1 1]);
asserts.isequal(size(ksstat), [1 1]);
times = zeros(1, 5);
for k = 1:5
  tic();
  [h, p, ksstat] = kstest(x);
  times(k) = toc();
  asserts.isequal(size(h), [1 1]);
  asserts.isequal(size(p), [1 1]);
  asserts.isequal(size(ksstat), [1 1]);
end
disp(['kstest observations: ', int2str(n)]);
disp(['kstest best time: ', num2str(min(times)), ' s']);
disp(['kstest median time: ', num2str(median(times)), ' s']);
disp(['kstest mean time: ', num2str(mean(times)), ' s']);
%=============================================================================
