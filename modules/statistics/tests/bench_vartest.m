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
rows = 20000;
cols = 50;
x = randn(rows, cols);
[h, p] = vartest(x, 1, 'Dim', 1);
asserts.isequal(size(h), [1 cols]);
asserts.isequal(size(p), [1 cols]);
times = zeros(1, 5);
for k = 1:5
  tic();
  [h, p] = vartest(x, 1, 'Dim', 1);
  times(k) = toc();
  asserts.isequal(size(h), [1 cols]);
  asserts.isequal(size(p), [1 cols]);
end
disp(['vartest matrix: ', int2str(rows), 'x', int2str(cols)]);
disp(['vartest best time: ', num2str(min(times)), ' s']);
disp(['vartest median time: ', num2str(median(times)), ' s']);
disp(['vartest mean time: ', num2str(mean(times)), ' s']);
%=============================================================================
