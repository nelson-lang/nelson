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
R = sparse([1 0; 0 1; 1 1; 2 -1]);
c = [1; 2; 4; 1];
REF_DEFAULT_RESVEC = [4.6904157598234297; 1.9197965332547597; 0.64168894791974773];
REF_DEFAULT_LSVEC = [4.2795304965501781; 1.2371237006114824];
Rs = sparse(single([1 0; 0 1; 1 1; 2 -1]));
cs = single([1; 2; 4; 1]);
[xs, flags, relress, iters, resvecs, lsvecs] = lsqr(Rs, cs, 1e-6, 20);
asserts.isequal(class(xs), 'single');
asserts.isequal(flags, 0);
asserts.isapprox(relress, 0.13680854337396819, 1e-6);
asserts.isequal(iters, 2);
asserts.isapprox(resvecs, [4.6904157598234297; 1.9197965332547597; 0.64168894791974773], 1e-5);
asserts.isapprox(lsvecs, [4.2795304965501781; 1.2371237006114824; 0], 1e-5);
asserts.isapprox(xs, single([1.5294117647058829; 2.1764705882352953]), 1e-5);
asserts.isapprox(norm(Rs * xs - cs), resvecs(end), 1e-5);
%=============================================================================
Ds = single([1; 2]);
[xjs, flagjs, relresjs, iterjs] = lsqr(Rs, cs, 1e-6, 20, Ds);
asserts.isequal(class(xjs), 'single');
asserts.isequal(flagjs, 0);
asserts.isapprox(relresjs, 0.13680854337396831, 1e-6);
asserts.isequal(iterjs, 2);
asserts.isapprox(norm(Rs * xjs - cs), 0.64168894791974818, 1e-5);
%=============================================================================
[xfun, flagfun, relresfun, iterfun, resvecfun, lsvecfun] = lsqr(R, c, 1e-12, 20, @(x, t) x);
asserts.isequal(flagfun, 0);
asserts.isapprox(relresfun, 0.13680854337396819, 1e-12);
asserts.isequal(iterfun, 2);
asserts.isapprox(resvecfun, REF_DEFAULT_RESVEC, 1e-12);
asserts.isapprox(lsvecfun(1:2), REF_DEFAULT_LSVEC, 1e-12);
asserts.istrue(lsvecfun(end) < 1e-12);
asserts.isapprox(xfun, [1.5294117647058829; 2.1764705882352953], 1e-12);
asserts.isapprox(norm(R * xfun - c), resvecfun(end), 1e-10);
%=============================================================================
[xfun2, flagfun2, relresfun2, iterfun2, resvecfun2, lsvecfun2] = lsqr(R, c, 1e-12, 20, [], @(x, t) x);
asserts.isequal(flagfun2, 0);
asserts.isapprox(relresfun2, 0.13680854337396819, 1e-12);
asserts.isequal(iterfun2, 2);
asserts.isapprox(resvecfun2, REF_DEFAULT_RESVEC, 1e-12);
asserts.isapprox(lsvecfun2(1:2), REF_DEFAULT_LSVEC, 1e-12);
asserts.istrue(lsvecfun2(end) < 1e-12);
asserts.isapprox(xfun2, [1.5294117647058829; 2.1764705882352953], 1e-12);
asserts.isapprox(norm(R * xfun2 - c), resvecfun2(end), 1e-10);
%=============================================================================
[xfuns, flagfuns, relresfuns, iterfuns] = lsqr(R, c, 1e-12, 20, @(x, t) sparse(x));
asserts.isequal(flagfuns, 0);
asserts.isapprox(relresfuns, 0.13680854337396819, 1e-12);
asserts.isequal(iterfuns, 2);
asserts.isapprox(xfuns, [1.5294117647058829; 2.1764705882352953], 1e-12);
asserts.isapprox(norm(R * xfuns - c), 0.64168894791974773, 1e-12);
%=============================================================================
