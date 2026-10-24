%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--REUSE PROCESS-->
A = sparse([4 1 0; 2 3 1; 0 1 2]);
C = sparse([3 + 1i 1; 0 2 - 1i]);
d = [4 + 2i; 3 - 1i];
[y, cflag, crelres, citer, cresvec, clsvec] = lsmr(C, d, 1e-12, 20);
asserts.isequal(cflag, 0);
asserts.istrue(crelres < 1e-12);
asserts.isequal(citer, 2);
asserts.isequal(size(clsvec), size(cresvec));
asserts.isapprox(cresvec(1:2), [5.4772255750516612; 1.094257868048037], 1e-12);
asserts.istrue(cresvec(end) < 1e-12);
asserts.isapprox(clsvec(1:2), [18.165902124584946; 2.26184919939826], 1e-12);
asserts.istrue(clsvec(end) < 1e-12);
asserts.isapprox(y, [0.96 + 0.28i; 1.4 + 0.2i], 1e-12);
asserts.isapprox(C * y, d, 1e-10);
%=============================================================================
Cs = sparse(single([3 + 1i 1; 0 2 - 1i]));
ds = single([4 + 2i; 3 - 1i]);
[ys, cflags, crelress, citers, cresvecs, clsvecs] = lsmr(Cs, ds, 1e-6, 20);
asserts.isequal(class(ys), 'single');
asserts.isequal(cflags, 0);
asserts.istrue(crelress < 1e-6);
asserts.isequal(citers, 2);
asserts.isapprox(cresvecs, [5.4772255750516612; 1.094257868048037; 0], 1e-5);
asserts.isapprox(clsvecs, [18.165902124584946; 2.26184919939826; 0], 1e-5);
asserts.isapprox(ys, single([0.96 + 0.28i; 1.4 + 0.2i]), 1e-5);
asserts.isapprox(Cs * ys, ds, 1e-5);
%=============================================================================
Dreal = diag([3; 2]);
[yr, rflag, rrelres, riter, rresvec, rlsvec] = lsmr(C, d, 1e-12, 20, Dreal);
asserts.isequal(rflag, 0);
asserts.istrue(rrelres < 1e-12);
asserts.isequal(riter, 2);
asserts.isapprox(rresvec(1:2), [5.4772255750516612; 0.6452985875304], 1e-12);
asserts.istrue(rresvec(end) < 1e-12);
asserts.isequal(size(rlsvec), size(rresvec));
asserts.isapprox(rlsvec(1:2), [7.397447007057381; 0.556947614123459], 1e-12);
asserts.istrue(rlsvec(end) < 1e-12);
asserts.isapprox(yr, [0.96 + 0.28i; 1.4 + 0.2i], 1e-12);
asserts.isapprox(C * yr, d, 1e-10);
%=============================================================================
bc = [1 + 1i; 2; 3 - 1i];
[ymix1, mixflag1, mixrelres1, mixiter1, mixresvec1, mixlsvec1] = lsmr(A, bc, 1e-12, 20);
asserts.isequal(mixflag1, 0);
asserts.istrue(mixrelres1 < 1e-12);
asserts.isequal(mixiter1, 3);
asserts.isapprox(mixresvec1(1:3), [4; 2.301632396993634; 1.048150823648315], 1e-12);
asserts.istrue(mixresvec1(end) < 1e-12);
asserts.isequal(size(mixlsvec1), size(mixresvec1));
asserts.isapprox(mixlsvec1(1:3), [15.748015748023622; 5.45442340984481; 1.181064622633259], 1e-12);
asserts.istrue(mixlsvec1(end) < 1e-12);
asserts.isapprox(ymix1, [0.25 + 0.25i; 0; 1.5 - 0.5i], 1e-12);
asserts.isapprox(A * ymix1, bc, 1e-10);
%=============================================================================
br = [4; 3];
[ymix2, mixflag2, mixrelres2, mixiter2, mixresvec2, mixlsvec2] = lsmr(C, br, 1e-12, 20);
asserts.isequal(mixflag2, 0);
asserts.istrue(mixrelres2 < 1e-12);
asserts.isequal(mixiter2, 2);
asserts.isapprox(mixresvec2(1:2), [5; 1.147406588549267], 1e-12);
asserts.istrue(mixresvec2(end) < 1e-12);
asserts.isequal(size(mixlsvec2), size(mixresvec2));
asserts.isapprox(mixlsvec2(1:2), [16.401219466856727; 2.373173711452969], 1e-12);
asserts.istrue(mixlsvec2(end) < 1e-12);
asserts.isapprox(ymix2, [0.78 - 0.46i; 1.2 + 0.6i], 1e-12);
asserts.istrue(norm(C * ymix2 - br) < 1e-10);
%=============================================================================
