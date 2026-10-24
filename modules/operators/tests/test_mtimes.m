%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
R = [11 21 31] * [11; 21; 31];
REF = 1523;
asserts.isequal(R, REF);
%=============================================================================
R = [11; 21; 31] * [11 21 31];
REF = [121.0000 231.0000 341.0000;
  231.0000 441.0000 651.0000;
  341.0000 651.0000 961.0000];
asserts.isequal(R, REF);
%=============================================================================
M = [11 21; 31 41];
R = M * M;
REF = [772 1092; 1612 2332];
asserts.isequal(R, REF);
%=============================================================================
R = [11 21 31] * 2;
REF = [22 42 62];
asserts.isequal(R, REF);
%=============================================================================
R = [11; 21; 31] * 2;
REF = [22; 42; 62];
asserts.isequal(R, REF);
%=============================================================================
S = sparse([1 0; 2 3; 0 4]);
c = max(max(abs(S))) / 1000;
R = c * ones(3, 1);
REF = 0.004 * ones(3, 1);
asserts.istrue(issparse(R));
asserts.isequal(size(R), [3 1]);
asserts.isapprox(full(R), REF, 1e-12);
%=============================================================================
R = ones(1, 3) * c;
REF = 0.004 * ones(1, 3);
asserts.istrue(issparse(R));
asserts.isequal(size(R), [1 3]);
asserts.isapprox(full(R), REF, 1e-12);
%=============================================================================
R = [11 21i 31] * [11; 21; 31];
REF = 1082 + 441i;
asserts.isequal(R, REF);
%=============================================================================
R = [11; 21i; 31] * [11 21 31];
REF = [121.0000 231.0000 341.0000;
  0.0000 + 231.0000i 0.0000 + 441.0000i 0.0000 + 651.0000i;
  341.0000 651.0000 961.0000];
asserts.isequal(R, REF);
%=============================================================================
M = [11 21i; 31 41];
R = M * M;
REF = [121.0000 + 651.0000i, 0.0000 + 1092.0000i;
  1612.0000, 1681.0000 + 651.0000i];
asserts.isequal(R, REF);
%=============================================================================
R = [1 2i 3] * 231;
REF = [231 462i 693];
asserts.isequal(R, REF);
%=============================================================================
R = [13; 23i; 31] * 242;
REF = [3146; 5566i; 7502];
asserts.isequal(R, REF);
%=============================================================================
M = [11 21 31];
N = [11 21 31 41];
asserts.checkerror('M * N', _('Incorrect dimensions for matrix multiplication.'));
%=============================================================================
M = ones(3, 3, 3);
asserts.checkerror('M * M', sprintf(_('%s must be 2-D.'), 'Input arguments'));
%=============================================================================
A = 1:2;
B = (1:3)';
asserts.checkerror('R = A * B', _('Incorrect dimensions for matrix multiplication.'));
%=============================================================================
A = 1:2;
B = (1:3)';
R = B * A;
REF = [1 2; 2 4; 3 6];
asserts.isequal(R, REF);
%=============================================================================
R = intmax('int32') * intmax('int32');
REF = intmax('int32');
asserts.isequal(R, REF);
%=============================================================================
R = 1 * single(eps);
REF = single(eps);
asserts.isequal(R, REF);
%=============================================================================
R = single(eps) * 1;
REF = single(eps);
asserts.isequal(R, REF);
%=============================================================================
R = ones(3, 3) * single(eps);
REF = single(ones(3, 3) * eps);
asserts.isequal(R, REF);
%=============================================================================
R = single(eps) * ones(3, 3);
REF = single(ones(3, 3) * eps);
asserts.isequal(R, REF);
%=============================================================================
A = zeros(0, 3);
B = zeros(3, 1);
R = A * B;
asserts.isequal(size(R), [0 1]);
asserts.istrue(isempty(R));
asserts.isequal(class(R), 'double');
%=============================================================================
A = zeros(2, 0);
B = zeros(0, 4);
R = A * B;
REF = zeros(2, 4);
asserts.isequal(R, REF);
%=============================================================================
A = single(zeros(0, 3));
B = single(zeros(3, 1));
R = A * B;
asserts.isequal(size(R), [0 1]);
asserts.istrue(isempty(R));
asserts.isequal(class(R), 'single');
%=============================================================================
A = single(zeros(2, 0));
B = single(zeros(0, 4));
R = A * B;
REF = single(zeros(2, 4));
asserts.isequal(R, REF);
%=============================================================================
A = complex(zeros(0, 3), zeros(0, 3));
B = zeros(3, 1);
R = A * B;
asserts.isequal(size(R), [0 1]);
asserts.istrue(isempty(R));
%=============================================================================
A = sparse(zeros(0, 3));
B = sparse(zeros(3, 1));
R = A * B;
asserts.isequal(size(R), [0 1]);
asserts.istrue(isempty(R));
asserts.istrue(issparse(R));
%=============================================================================
A = sparse(zeros(2, 0));
B = sparse(zeros(0, 4));
R = A * B;
asserts.isequal(size(R), [2 4]);
asserts.isequal(full(R), zeros(2, 4));
asserts.istrue(issparse(R));
%=============================================================================
A = sparse(single([4 1 0; 2 3 1; 0 1 2]));
x = single([0.25; 0; 1.5]);
R = A * x;
asserts.isequal(class(R), 'single');
asserts.isfalse(issparse(R));
asserts.isapprox(R, single([1; 2; 3]), 1e-6);
%=============================================================================
B = sparse(single([1 0; 0 2; 3 0]));
R = A * B;
asserts.isequal(class(R), 'single');
asserts.istrue(issparse(R));
asserts.isapprox(full(R), single([4 2; 5 6; 6 2]), 1e-6);
%=============================================================================
C = sparse(single([3 + 1i 1; 0 2 - 1i]));
v = single([1 + 2i; 3 - 1i]);
R = C * v;
asserts.isequal(class(R), 'single');
asserts.isfalse(issparse(R));
asserts.isapprox(R, single([4 + 6i; 5 - 5i]), 1e-6);
%=============================================================================
asserts.checkerror('sparse(single([1 2])) * single([1; 2; 3])', _('Incorrect dimensions for matrix multiplication.'));
%=============================================================================
% large products (>= 65536 flops) take the threaded BLAS xgemm path; the small
% matrices above cover the Eigen fallback. Check the BLAS path against trivial
% and block-decomposition references, per type.
n = 300;
Ad = reshape(mod(1:n * n, 13), n, n) / 13;
Bd = reshape(mod(1:n * n, 7), n, n) / 7;
asserts.isapprox(Ad * eye(n), Ad, 1e-12);
asserts.isapprox(eye(n) * Ad, Ad, 1e-12);
Cd = Ad * Bd;
Cref = Ad(:, 1:150) * Bd(1:150, :) + Ad(:, 151:n) * Bd(151:n, :);
asserts.isapprox(Cd, Cref, 1e-9);
asserts.isequal(size(Ad * Bd(:, 1)), [n 1]);
%=============================================================================
As = single(Ad);
Bs = single(Bd);
asserts.isequal(class(As * Bs), 'single');
asserts.isapprox(As * eye(n, 'single'), As, 1e-4);
Csref = As(:, 1:150) * Bs(1:150, :) + As(:, 151:n) * Bs(151:n, :);
asserts.isapprox(As * Bs, Csref, 1e-3);
%=============================================================================
Az = Ad + 1i * Bd;
Bz = Bd - 1i * Ad;
Cz = Az * Bz;
Czref = Az(:, 1:150) * Bz(1:150, :) + Az(:, 151:n) * Bz(151:n, :);
asserts.isapprox(Cz, Czref, 1e-9);
asserts.isapprox(Az * eye(n), Az, 1e-12);
%=============================================================================
