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
x = reshape(linspace(0, 1, n), [], 10);
times = zeros(1, 5);
for k = 1:5
  tic();
  [aHat, bHat, aCI, bCI] = unifit(x, 0.05);
  times(k) = toc();
  asserts.isequal(size(aHat), [1 10]);
  asserts.isequal(size(bHat), [1 10]);
  asserts.isequal(size(aCI), [2 10]);
  asserts.isequal(size(bCI), [2 10]);
end
disp(['unifit elements: ', int2str(n)]);
disp(['unifit best time: ', num2str(min(times)), ' s']);
disp(['unifit median time: ', num2str(median(times)), ' s']);
disp(['unifit mean time: ', num2str(mean(times)), ' s']);
%=============================================================================
