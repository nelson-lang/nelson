%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = sparse([1 0 2; 0 3 0]);
B = sparse([0 4 0; 5 0 6]);
%=============================================================================
R = A + B;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([1 4 2; 5 3 6]));
%=============================================================================
R = A - B;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([1 -4 2; -5 3 -6]));
%=============================================================================
R = A .* B;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(2, 3));
%=============================================================================
C = sparse([0 2; 3 0; 0 4]);
R = A * C;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([0 10; 9 0]));
%=============================================================================
D = [10 20 30; 40 50 60];
R = A + D;
asserts.isfalse(issparse(R));
asserts.isequal(R, [11 20 32; 40 53 60]);
%=============================================================================
R = D + A;
asserts.isfalse(issparse(R));
asserts.isequal(R, [11 20 32; 40 53 60]);
%=============================================================================
R = A - D;
asserts.isfalse(issparse(R));
asserts.isequal(R, [-9 -20 -28; -40 -47 -60]);
%=============================================================================
R = D - A;
asserts.isfalse(issparse(R));
asserts.isequal(R, [9 20 28; 40 47 60]);
%=============================================================================
R = A .* D;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([10 0 60; 0 150 0]));
%=============================================================================
R = D .* A;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([10 0 60; 0 150 0]));
%=============================================================================
Cfull = [0 2; 3 0; 0 4];
R = A * Cfull;
asserts.isfalse(issparse(R));
asserts.isequal(R, [0 10; 9 0]);
%=============================================================================
Afull = [1 0 2; 0 3 0];
R = Afull * C;
asserts.isfalse(issparse(R));
asserts.isequal(R, [0 10; 9 0]);
%=============================================================================
R = A .* 2;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([2 0 4; 0 6 0]));
%=============================================================================
R = A + 3;
asserts.isfalse(issparse(R));
asserts.isequal(R, [4 3 5; 3 6 3]);
%=============================================================================
R = 3 + A;
asserts.isfalse(issparse(R));
asserts.isequal(R, [4 3 5; 3 6 3]);
%=============================================================================
R = A - 3;
asserts.isfalse(issparse(R));
asserts.isequal(R, [-2 -3 -1; -3 0 -3]);
%=============================================================================
R = 3 - A;
asserts.isfalse(issparse(R));
asserts.isequal(R, [2 3 1; 3 0 3]);
%=============================================================================
R = A + 0;
asserts.isfalse(issparse(R));
asserts.isequal(R, [1 0 2; 0 3 0]);
%=============================================================================
R = 0 - A;
asserts.isfalse(issparse(R));
asserts.isequal(R, [-1 0 -2; 0 -3 0]);
%=============================================================================
R = A + sparse(3);
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([4 3 5; 3 6 3]));
%=============================================================================
R = sparse(3) - A;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([2 3 1; 3 0 3]));
%=============================================================================
R = 2 .* A;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([2 0 4; 0 6 0]));
%=============================================================================
R = A * 2;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([2 0 4; 0 6 0]));
%=============================================================================
R = 2 * A;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([2 0 4; 0 6 0]));
%=============================================================================
R = A ./ 2;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([0.5 0 1; 0 1.5 0]));
%=============================================================================
R = A / 2;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([0.5 0 1; 0 1.5 0]));
%=============================================================================
R = 2 .\ A;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([0.5 0 1; 0 1.5 0]));
%=============================================================================
R = 2 \ A;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([0.5 0 1; 0 1.5 0]));
%=============================================================================
R = A .^ 2;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([1 0 4; 0 9 0]));
%=============================================================================
As = sparse(single([2 0; 0 3]));
R = As .^ 2;
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'single');
asserts.isequal(full(R), single([4 0; 0 9]));
%=============================================================================
R = As .^ single(0);
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'single');
asserts.isequal(full(R), single(ones(2, 2)));
%=============================================================================
R = As .^ single(-1);
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'single');
asserts.isequal(full(R), single([0.5 Inf; Inf 1 / 3]));
%=============================================================================
Cs = sparse(single([1 + 2i 0; 0 3]));
R = Cs .^ 2;
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'single');
asserts.isapprox(full(R), single([-3 + 4i 0; 0 9]), 1e-6);
%=============================================================================
R = Cs .^ single(1 + 1i);
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'single');
REF = single(full(Cs) .^ single(1 + 1i));
asserts.isapprox(full(R), REF, 1e-6);
%=============================================================================
M = sparse([1 2; 3 4]);
R = M ^ 0;
asserts.istrue(issparse(R));
asserts.isequal(R, speye(2));
%=============================================================================
R = M ^ 1;
asserts.istrue(issparse(R));
asserts.isequal(R, M);
%=============================================================================
R = M ^ 2;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([7 10; 15 22]));
%=============================================================================
R = M ^ 3;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([37 54; 81 118]));
%=============================================================================
R = M ^ 3.5;
asserts.istrue(issparse(R));
REF = sparse([85.8502958724692 - 0.0239607550329477i, ...
    125.120548413439 + 0.0109602988743794i; ...
    187.680822620158 + 0.0164404483115691i, ...
    273.531118492627 - 0.00752030672137863i]);
asserts.isapprox(full(R), full(REF), 1e-9);
%=============================================================================
R = 2 ^ M;
asserts.isfalse(issparse(R));
REF = [10.4827393896286 14.1518788283206; ...
    21.227818242481 31.7105576321096];
asserts.isapprox(R, REF, 1e-9);
%=============================================================================
Dg = sparse([1 0; 0 2]);
R = Dg ^ -1;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([1 0; 0 0.5]));
%=============================================================================
E = sparse([1 0; 0 2]);
F = sparse([0 4; 5 0]);
R = E ./ F;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([Inf 0; 0 Inf]));
%=============================================================================
R = F ./ E;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([0 Inf; Inf 0]));
%=============================================================================
H = sparse([1 0 0; 0 2 0; 0 0 0]);
I = sparse([0 4 0; 5 0 0; 0 0 0]);
R = H ./ I;
asserts.istrue(issparse(R));
asserts.isequal(nnz(R), 7);
asserts.isequal(isnan(full(R)), logical([false false true; false false true; true true true]));
%=============================================================================
R = [1 0; 0 2] ./ F;
asserts.isfalse(issparse(R));
asserts.isequal(R, [Inf 0; 0 Inf]);
%=============================================================================
R = E ./ [0 4; 5 0];
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([Inf 0; 0 Inf]));
%=============================================================================
Xs = sparse(single([1 + 2i 0; 0 3 - 4i]));
Ys = sparse(single([2 - 1i 5i; 6 1i]));
R = Xs ./ Ys;
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'single');
asserts.isapprox(full(R), single([1i 0; 0 -4 - 3i]), 1e-6);
%=============================================================================
R = Ys ./ Xs;
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'single');
F = full(R);
asserts.isapprox(F(1, 1), single(-1i), 1e-6);
asserts.istrue(isinf(imag(F(1, 2))) && imag(F(1, 2)) > 0);
asserts.istrue(isinf(real(F(2, 1))) && real(F(2, 1)) > 0);
asserts.isapprox(F(2, 2), single(-0.16 + 0.12i), 1e-6);
%=============================================================================
Yfulls = single([2 - 1i 5i; 6 1i]);
R = Xs ./ Yfulls;
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'single');
asserts.isapprox(full(R), single([1i 0; 0 -4 - 3i]), 1e-6);
%=============================================================================
R = Yfulls ./ Xs;
asserts.isfalse(issparse(R));
asserts.isequal(class(R), 'single');
asserts.isapprox(R(1, 1), single(-1i), 1e-6);
asserts.istrue(isinf(imag(R(1, 2))) && imag(R(1, 2)) > 0);
asserts.istrue(isinf(real(R(2, 1))) && real(R(2, 1)) > 0);
asserts.isapprox(R(2, 2), single(-0.16 + 0.12i), 1e-6);
%=============================================================================
R = E .^ 0;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(ones(2, 2)));
%=============================================================================
R = E .^ -1;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([1 Inf; Inf 0.5]));
%=============================================================================
S = sparse([4 0; 0 2]);
T = sparse([8; 6]);
R = S \ T;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([2; 3]));
%=============================================================================
R = S \ [8; 6];
asserts.isfalse(issparse(R));
asserts.isequal(R, [2; 3]);
%=============================================================================
U = sparse([6 2; 4 1]);
R = U \ [4; 9];
asserts.isfalse(issparse(R));
asserts.isapprox(R, [7; -19], 1e-12);
%=============================================================================
R = U \ sparse([4; 9]);
asserts.istrue(issparse(R));
asserts.isapprox(full(R), [7; -19], 1e-12);
%=============================================================================
Arect = sparse([1 2 0; 0 4 3]);
R = Arect \ sparse([8; 18]);
asserts.istrue(issparse(R));
asserts.isapprox(full(R), [8; 0; 6], 1e-12);
asserts.isapprox(full(Arect * R), [8; 18], 1e-12);
%=============================================================================
R = [4 0; 0 2] \ T;
asserts.isfalse(issparse(R));
asserts.isequal(R, [2; 3]);
%=============================================================================
V = sparse([8 6; 4 10]);
R = V / S;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([2 3; 1 5]));
%=============================================================================
R = [8 6; 4 10] / S;
asserts.isfalse(issparse(R));
asserts.isequal(R, [2 3; 1 5]);
%=============================================================================
Z = sparse(0);
R = A + Z;
asserts.istrue(issparse(R));
asserts.isequal(R, A);
%=============================================================================
R = Z - A;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([-1 0 -2; 0 -3 0]));
%=============================================================================
L1 = sparse(logical([true false; false true]));
L2 = sparse(logical([true true; false false]));
R = L1 + L2;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([2 1; 0 1]));
%=============================================================================
R = L1 - L2;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([0 -1; 0 1]));
%=============================================================================
R = L1 .* L2;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([true false; false false])));
%=============================================================================
Lfull = logical([false true; true false]);
R = L1 + Lfull;
asserts.isfalse(issparse(R));
asserts.isequal(R, [1 1; 1 1]);
%=============================================================================
R = Lfull - L1;
asserts.isfalse(issparse(R));
asserts.isequal(R, [-1 1; 1 -1]);
%=============================================================================
R = L1 .* Lfull;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([false false; false false])));
%=============================================================================
X = sparse([1 + 2i 0; 0 3 - 4i]);
Y = sparse([0 5i; 6 0]);
R = X + Y;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([1 + 2i 5i; 6 3 - 4i]));
%=============================================================================
R = Xs + Ys;
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'single');
asserts.isapprox(full(R), single([3 + 1i 5i; 6 3 - 3i]), 1e-6);
%=============================================================================
R = Ys - Xs;
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'single');
asserts.isapprox(full(R), single([1 - 3i 5i; 6 -3 + 5i]), 1e-6);
%=============================================================================
R = -Xs;
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'single');
asserts.isapprox(full(R), single([-1 - 2i 0; 0 -3 + 4i]), 1e-6);
%=============================================================================
R = Xs .* Ys;
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'single');
asserts.isapprox(full(R), single([4 + 3i 0; 0 4 + 3i]), 1e-6);
%=============================================================================
Yfull = [0 5i; 6 0];
R = X + Yfull;
asserts.isfalse(issparse(R));
asserts.isequal(R, [1 + 2i 5i; 6 3 - 4i]);
%=============================================================================
R = Yfull - X;
asserts.isfalse(issparse(R));
asserts.isequal(R, [-1 - 2i 5i; 6 -3 + 4i]);
%=============================================================================
R = X * Y;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([0 -10 + 5i; 18 - 24i 0]));
%=============================================================================
R = X * Yfull;
asserts.isfalse(issparse(R));
asserts.isequal(R, [0 -10 + 5i; 18 - 24i 0]);
%=============================================================================
