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
n = 1000000;
a = zeros(n, 1);
b = linspace(1, 2, n)';
times = zeros(1, 5);
for k = 1:5
  tic();
  [m, v] = unifstat(a, b);
  times(k) = toc();
  asserts.isequal(size(m), [n 1]);
  asserts.isequal(size(v), [n 1]);
end
disp(['unifstat elements: ', int2str(n)]);
disp(['unifstat best time: ', num2str(min(times)), ' s']);
disp(['unifstat median time: ', num2str(median(times)), ' s']);
disp(['unifstat mean time: ', num2str(mean(times)), ' s']);
%=============================================================================
