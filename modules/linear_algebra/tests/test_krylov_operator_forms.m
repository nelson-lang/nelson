%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% iterative solvers accept sparse, full and function handle forms of A
n = 12;
e = ones(n, 1);
A = full(spdiags([-1 * e, 3 * e, -0.5 * e], -1:1, n, n));
Asym = full(spdiags([-1 * e, 3 * e, -1 * e], -1:1, n, n));
b = (1:n).';
Ar = [A; ones(2, n)];
br = [b; 1; 2];
names = {'pcg', 'minres', 'bicg', 'bicgstab', 'cgs', 'qmr', 'gmres', 'lsqr', 'lsmr'};
withTranspose = {'bicg', 'qmr', 'lsqr', 'lsmr'};
for k = 1:numel(names)
  f = str2func(names{k});
  M = A;
  rhs = b;
  if any(strcmp(names{k}, {'pcg', 'minres'}))
    M = Asym;
  end
  if any(strcmp(names{k}, {'lsqr', 'lsmr'}))
    M = Ar;
    rhs = br;
  end
  extra = {1e-10, 40};
  if strcmp(names{k}, 'gmres')
    extra = {[], 1e-10, 40};
  end
  if any(strcmp(names{k}, withTranspose))
    afun = @(x, flag) applyOperator(M, x, flag);
  else
    afun = @(x) M * x;
  end
  [xs, fs, rs, its] = f(sparse(M), rhs, extra{:});
  [xf, ff, rf, itf] = f(M, rhs, extra{:});
  [xh, fh, rh, ith] = f(afun, rhs, extra{:});
  assert_isequal([fs, ff, fh], [0 0 0]);
  assert_isequal(its, itf);
  assert_isequal(its, ith);
  assert_isapprox(xf, xs, 1e-12);
  assert_isapprox(xh, xs, 1e-12);
end
%=============================================================================
% preconditioner given as a function handle with a function handle matrix
x = pcg(@(v) Asym * v, b, 1e-10, 40, @(v) v / 3);
assert_istrue(norm(Asym * x - b) < 1e-10);
%=============================================================================
% full single data stays single
x = pcg(single(Asym), single(b));
assert_isequal(class(x), 'single');
x = cgs(single(A), single(b), 1e-5, 40);
assert_isequal(class(x), 'single');
%=============================================================================
assert_checkerror('pcg(ones(3, 4), ones(3, 1))', _('Square matrix expected.'));
assert_checkerror('pcg(@(x) [x; 1], ones(3, 1))', _('Matrix and vector dimensions must agree.'));
%=============================================================================
function y = applyOperator(M, x, flag)
  % product with M or its transpose, as asked by the solver
  if strcmp(flag, 'transp')
    y = M' * x;
  else
    y = M * x;
  end
end
%=============================================================================
