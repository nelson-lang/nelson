%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% nnz on a dense array large enough to be counted in parallel must give the
% exact count, every time (the parallel count used to race on its accumulator).
%=============================================================================
initialEnabled = feature('ompEnabled');
initialThreshold = feature('ompThreshold');
feature('ompEnabled', true);
feature('ompThreshold', 0);
n = 1000;
for k = 1:5
  asserts.isequal(nnz(true(n)), n * n);
  asserts.isequal(nnz(ones(n)), n * n);
  asserts.isequal(nnz(ones(n, 'int8')), n * n);
  asserts.isequal(nnz(ones(n, 'single')), n * n);
  asserts.isequal(nnz(true(1, n * n)), n * n);
end
%=============================================================================
rng(42, 'twister');
x = rand(598, 600) > 0.5;
asserts.isequal(nnz(x), sum(x(:)));
asserts.isequal(nnz(x), nnz(x));
d = rand(598, 600);
d(d < 0.3) = 0;
asserts.isequal(nnz(d), sum(d(:) ~= 0));
%=============================================================================
z = complex(rand(500, 500), rand(500, 500));
z(1:2:end, :) = 0;
asserts.isequal(nnz(z), sum(z(:) ~= 0));
%=============================================================================
feature('ompEnabled', initialEnabled);
feature('ompThreshold', initialThreshold);
%=============================================================================
