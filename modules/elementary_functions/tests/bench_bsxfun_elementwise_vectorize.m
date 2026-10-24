%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--RELEASE ONLY-->
%=============================================================================
% Bench: bsxfun auto-vectorization for element-wise anonymous callbacks.
%
% bsxfun already evaluates its callback once (not per element), but the general
% path materializes BOTH operands to the full output size with repmat before
% calling the callback. When the callback is a provably element-wise,
% broadcasting-safe body (or a broadcasting named handle such as @plus), the
% fast path skips both repmat calls and relies on Nelson's implicit operator
% broadcasting. This bench reports, per size, the repmat-based path vs the fast
% path for the identical operation:
%   - repmat : general path, forced with a body carrying a matrix operator
%              (`* 1`, an identity here) so it is not treated as element-wise
%              and both operands are expanded with repmat
%   - vect   : the auto-vectorized fast path (element-wise body / named handle)
%   - floor  : the hand-vectorized broadcasting expression (lower bound)
%=============================================================================
sizes = [500, 1000, 2000, 3000]; % A is n x 1, B is 1 x n, result is n x n
fprintf('  bsxfun add on n x 1 (+) 1 x n  ->  n x n\n');
fprintf('  %8s %12s %12s %12s %10s\n', 'n', 'repmat(s)', 'vect(s)', 'floor(s)', 'speedup');
repmatBody = @(x, y) x + y * 1; % `* 1` forces the repmat path, identity result
for n = sizes
  A = rand(n, 1);
  B = rand(1, n);
  ref = A + B;
  % correctness: repmat path, fast path (body and named handle) and the hand
  % vectorized expression must all agree.
  asserts.isequal(bsxfun(repmatBody, A, B), ref);
  asserts.isequal(bsxfun(@(x, y) x + y, A, B), ref);
  asserts.isequal(bsxfun(@plus, A, B), ref);
  tic(); bsxfun(repmatBody, A, B); t_repmat = toc(); % forced -> repmat path
  tic(); bsxfun(@plus, A, B); t_vect = toc(); % named handle -> fast path
  tic(); A + B; t_floor = toc();
  fprintf('  %8d %12.5f %12.5f %12.5f %9.1fx\n', ...
    n, t_repmat, t_vect, t_floor, t_repmat / max(t_vect, eps));
end
%=============================================================================
fprintf('\n  bsxfun @(x,y) sqrt(x.^2 + y.^2)  on n x 1 (,) 1 x n\n');
fprintf('  %8s %12s %12s %10s\n', 'n', 'vect(s)', 'floor(s)', 'ratio');
for n = sizes
  A = rand(n, 1);
  B = rand(1, n);
  rv = bsxfun(@(x, y) sqrt(x .^ 2 + y .^ 2), A, B);
  rf = sqrt(A .^ 2 + B .^ 2);
  asserts.isequal(rv, rf);
  tic(); bsxfun(@(x, y) sqrt(x .^ 2 + y .^ 2), A, B); t_vect = toc();
  tic(); sqrt(A .^ 2 + B .^ 2); t_floor = toc();
  fprintf('  %8d %12.5f %12.5f %9.2fx\n', n, t_vect, t_floor, t_vect / max(t_floor, eps));
end
%=============================================================================
