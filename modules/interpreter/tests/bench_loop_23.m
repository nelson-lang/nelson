%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
runs = 30;
cumulate = [];
b = 0;
N = 220;
for i = 1:runs
  b = zeros(N, N);
  tic();
  for j = 1:N
    for k = 1:N
      b(k,j) = abs(j - k) + 1;
    end
  end
  timing = toc();
  cumulate = [cumulate, timing];
end
timing = mean(cumulate);
disp(sum(cumulate))
%=============================================================================
