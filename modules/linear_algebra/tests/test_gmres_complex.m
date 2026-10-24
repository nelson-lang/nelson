%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
A = sparse([4 1 0; 2 3 1; 0 1 2]);
C = sparse([3 + 1i 1; 0 2 - 1i]);
d = [4 + 2i; 3 - 1i];
[y, cflag, crelres, citer, cresvec] = gmres(C, d, [], 1e-12, 20);
asserts.isequal(cflag, 0);
asserts.istrue(crelres < 1e-12);
asserts.isequal(citer, [1 2]);
asserts.isapprox(cresvec, [5.4772255750516612; 1.7320508075688767; 0], 1e-12);
asserts.isapprox(y, [0.96 + 0.28i; 1.4 + 0.2i], 1e-12);
asserts.isapprox(C * y, d, 1e-10);
%=============================================================================
Cs = sparse(single([3 + 1i 1; 0 2 - 1i]));
ds = single([4 + 2i; 3 - 1i]);
[ys, cflags, crelress, citers, cresvecs] = gmres(Cs, ds, [], 1e-6, 20);
asserts.isequal(class(ys), 'single');
asserts.isequal(cflags, 0);
asserts.istrue(crelress < 1e-6);
asserts.isequal(citers, [1 2]);
asserts.isapprox(cresvecs, single([5.4772255750516612; 1.7320508075688767; 0]), 1e-5);
asserts.isapprox(ys, single([0.96 + 0.28i; 1.4 + 0.2i]), 1e-5);
asserts.isapprox(Cs * ys, ds, 1e-5);
%=============================================================================
[yfs, flagfs, relresfs, iterfs] = gmres(Cs, ds, [], 1e-6, 20, @(x) sparse(x));
asserts.isequal(class(yfs), 'single');
asserts.isequal(flagfs, 0);
asserts.istrue(relresfs < 1e-6);
asserts.isequal(iterfs, [1 2]);
asserts.isapprox(Cs * yfs, ds, 1e-5);
%=============================================================================
Dreal = diag([3; 2]);
[yr, rflag, rrelres, riter, rresvec] = gmres(C, d, [], 1e-12, 20, Dreal);
asserts.isequal(rflag, 0);
asserts.istrue(rrelres < 1e-12);
asserts.isequal(riter, [1 2]);
asserts.isapprox(rresvec(1:2), [2.173067468400883; 0.581712756970257], 1e-12);
asserts.istrue(rresvec(end) < 1e-12);
asserts.isapprox(yr, [0.96 + 0.28i; 1.4 + 0.2i], 1e-12);
asserts.isapprox(C * yr, d, 1e-10);
%=============================================================================
bc = [1 + 1i; 2; 3 - 1i];
[ymix1, mixflag1, mixrelres1, mixiter1, mixresvec1] = gmres(A, bc, [], 1e-12, 20);
asserts.isequal(mixflag1, 0);
asserts.istrue(mixrelres1 < 1e-12);
asserts.isequal(mixiter1, [1 3]);
asserts.isapprox(mixresvec1(1:3), [4; 1.443137078762504; 0.525849935277722], 1e-12);
asserts.istrue(mixresvec1(end) < 1e-12);
asserts.isapprox(ymix1, [0.25 + 0.25i; 0; 1.5 - 0.5i], 1e-12);
asserts.isapprox(A * ymix1, bc, 1e-10);
%=============================================================================
br = [4; 3];
[ymix2, mixflag2, mixrelres2, mixiter2, mixresvec2] = gmres(C, br, [], 1e-12, 20);
asserts.isequal(mixflag2, 0);
asserts.istrue(mixrelres2 < 1e-12);
asserts.isequal(mixiter2, [1 2]);
asserts.isapprox(mixresvec2(1:2), [5; 1.885721097072432], 1e-12);
asserts.istrue(mixresvec2(end) < 1e-12);
asserts.isapprox(ymix2, [0.78 - 0.46i; 1.2 + 0.6i], 1e-12);
asserts.istrue(norm(C * ymix2 - br) < 1e-10);
%=============================================================================
