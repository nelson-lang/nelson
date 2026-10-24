%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--REUSE PROCESS-->
%=============================================================================
n = 1000000;
times = zeros(1, 5);
for k = 1:5
  rng(0);
  tic();
  x = lognrnd(0, 1, n, 1);
  times(k) = toc();
  asserts.isequal(size(x), [n 1]);
  asserts.istrue(all(x > 0));
end
disp(['lognrnd elements: ', int2str(n)]);
disp(['lognrnd best time: ', num2str(min(times)), ' s']);
disp(['lognrnd median time: ', num2str(median(times)), ' s']);
disp(['lognrnd mean time: ', num2str(mean(times)), ' s']);
%=============================================================================
