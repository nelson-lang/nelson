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
x = randn(n, 1);
referenceSize = size(x);
y = normcdf(x);
asserts.isequal(size(y), referenceSize);
times = zeros(1, 5);
for k = 1:5
  tic();
  y = normcdf(x);
  times(k) = toc();
  asserts.isequal(size(y), referenceSize);
end
disp(['normcdf elements: ', int2str(n)]);
disp(['normcdf best time: ', num2str(min(times)), ' s']);
disp(['normcdf median time: ', num2str(median(times)), ' s']);
disp(['normcdf mean time: ', num2str(mean(times)), ' s']);
%=============================================================================
