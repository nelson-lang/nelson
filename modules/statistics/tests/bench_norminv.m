%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--REUSE PROCESS-->
%=============================================================================
n = 1000000;
p = 0.001 + 0.998 * rand(n, 1);
referenceSize = size(p);
x = norminv(p);
asserts.isequal(size(x), referenceSize);
times = zeros(1, 5);
for k = 1:5
  tic();
  x = norminv(p);
  times(k) = toc();
  asserts.isequal(size(x), referenceSize);
end
disp(['norminv elements: ', int2str(n)]);
disp(['norminv best time: ', num2str(min(times)), ' s']);
disp(['norminv median time: ', num2str(median(times)), ' s']);
disp(['norminv mean time: ', num2str(mean(times)), ' s']);
%=============================================================================
