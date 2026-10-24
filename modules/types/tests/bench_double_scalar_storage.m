%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
N = 1000000;
samples = zeros(1, 9);
for repetition = 1:numel(samples)
  a = 0;
  b = 1;
  tic;
  for k = 1:N
    c = a + b;
    a = b;
    b = c;
  end
  samples(repetition) = toc;
end
asserts.isequal(k, N);
asserts.isequal(a, Inf);
asserts.isequal(b, Inf);
fprintf('Scalar recurrence: %.9f s (median)\n', median(samples));
%=============================================================================
