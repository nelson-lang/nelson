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
x = 0:(n - 1);
y = linspace(0, 0.999, n);
times = zeros(1, 5);
for k = 1:5
  tic();
  p = geopdf(x, 0.25);
  c = geocdf(x, 0.25);
  q = geoinv(y, 0.25);
  [m, v] = geostat(linspace(0.01, 1, n));
  times(k) = toc();
  asserts.isequal(size(p), [1 n]);
  asserts.isequal(size(c), [1 n]);
  asserts.isequal(size(q), [1 n]);
  asserts.isequal(size(m), [1 n]);
  asserts.isequal(size(v), [1 n]);
end
disp(['geometric distribution elements: ', int2str(n)]);
disp(['geometric distribution best time: ', num2str(min(times)), ' s']);
disp(['geometric distribution median time: ', num2str(median(times)), ' s']);
disp(['geometric distribution mean time: ', num2str(mean(times)), ' s']);
%=============================================================================
