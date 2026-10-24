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
% Bench: arrayfun auto-vectorization for element-wise anonymous callbacks.
%
% When the body is provably element-wise, arrayfun evaluates it ONCE on the full
% arrays instead of running the VM per element. This bench reports, per size:
%   - loop  : the previous per-element path (a body forced to fall back with a
%             trailing `* 1`, an identity on the scalar element that carries a
%             matrix operator so the body is not vectorizable)
%   - vect  : the auto-vectorized fast path
%   - floor : the hand-vectorized expression (lower bound)
% The three results are asserted equal first, then the speedup is printed.
%=============================================================================
sizes = [1e4, 1e5, 1e6];
fprintf('  arrayfun @(x,y) sqrt(x.^2 + y.^2)\n');
fprintf('  %10s %12s %12s %12s %10s\n', 'N', 'loop(s)', 'vect(s)', 'floor(s)', 'speedup');
for n = sizes
  A = rand(1, n);
  B = rand(1, n);
  % correctness: the forced-fallback body, the fast path and the hand
  % vectorized expression must all agree.
  loopBody = @(x, y) sqrt(x .^ 2 + y .^ 2) * 1; % `* 1` forces the per-element path
  vectBody = @(x, y) sqrt(x .^ 2 + y .^ 2);
  rl = arrayfun(loopBody, A, B);
  rv = arrayfun(vectBody, A, B);
  rf = sqrt(A .^ 2 + B .^ 2);
  % the fast path is bit-identical to the hand-vectorized expression; the old
  % per-element path may differ by a rounding unit (scalar vs array powers).
  asserts.isequal(rv, rf);
  asserts.istrue(max(abs(rl(:) - rf(:))) <= 1e-9);
  % time each
  tic(); arrayfun(loopBody, A, B); t_loop = toc();
  tic(); arrayfun(vectBody, A, B); t_vect = toc();
  tic(); sqrt(A .^ 2 + B .^ 2); t_floor = toc();
  fprintf('  %10d %12.5f %12.5f %12.5f %9.1fx\n', ...
    n, t_loop, t_vect, t_floor, t_loop / max(t_vect, eps));
end
%=============================================================================
fprintf('\n  arrayfun @(x) sin(x) + 1\n');
fprintf('  %10s %12s %12s %12s %10s\n', 'N', 'loop(s)', 'vect(s)', 'floor(s)', 'speedup');
for n = sizes
  A = rand(1, n);
  loopBody = @(x)(sin(x) + 1) * 1;
  vectBody = @(x) sin(x) + 1;
  asserts.isequal(arrayfun(vectBody, A), sin(A) + 1);
  asserts.istrue(max(abs(arrayfun(loopBody, A)(:) - (sin(A) + 1)(:))) <= 1e-12);
  tic(); arrayfun(loopBody, A); t_loop = toc();
  tic(); arrayfun(vectBody, A); t_vect = toc();
  tic(); sin(A) + 1; t_floor = toc();
  fprintf('  %10d %12.5f %12.5f %12.5f %9.1fx\n', ...
    n, t_loop, t_vect, t_floor, t_loop / max(t_vect, eps));
end
%=============================================================================
% Fast-path only, at a large size where the per-element loop is impractical.
n = 4e6;
A = rand(1, n);
B = rand(1, n);
tic(); rv = arrayfun(@(x, y) sqrt(x .^ 2 + y .^ 2), A, B); t_vect = toc();
asserts.isequal(rv, sqrt(A .^ 2 + B .^ 2));
fprintf('\n  arrayfun vectorized at N=%d: %.5f s\n', n, t_vect);
%=============================================================================
