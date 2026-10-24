%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 16-->
% <--REUSE PROCESS-->
%=============================================================================
n = 10000;
times = zeros(1, 5);
for k = 1:5
  rng(0);
  tic();
  a = chi2rnd(4, n, 1);
  b = trnd(5, n, 1);
  c = frnd(5, 7, n, 1);
  times(k) = toc();
  asserts.isequal(size(a), [n 1]);
  asserts.isequal(size(b), [n 1]);
  asserts.isequal(size(c), [n 1]);
end
disp(['inverse random distribution elements: ', int2str(n)]);
disp(['inverse random distribution best time: ', num2str(min(times)), ' s']);
disp(['inverse random distribution median time: ', num2str(median(times)), ' s']);
disp(['inverse random distribution mean time: ', num2str(mean(times)), ' s']);
%=============================================================================
