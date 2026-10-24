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
x = linspace(0, 10, n);
p = linspace(0, 0.999, n);
times = zeros(1, 5);
for k = 1:5
  tic();
  y = wblpdf(x, 2, 3);
  c = wblcdf(x, 2, 3);
  q = wblinv(p, 2, 3);
  [m, v] = wblstat(x + 1, 2);
  times(k) = toc();
  asserts.isequal(size(y), [1 n]);
  asserts.isequal(size(c), [1 n]);
  asserts.isequal(size(q), [1 n]);
  asserts.isequal(size(m), [1 n]);
  asserts.isequal(size(v), [1 n]);
end
disp(['weibull distribution elements: ', int2str(n)]);
disp(['weibull distribution best time: ', num2str(min(times)), ' s']);
disp(['weibull distribution median time: ', num2str(median(times)), ' s']);
disp(['weibull distribution mean time: ', num2str(mean(times)), ' s']);
%=============================================================================
