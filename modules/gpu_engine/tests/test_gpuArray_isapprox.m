%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite(~canUseGPU(), 'No compatible GPU device found.');
%=============================================================================
A = gpuArray(single([1 2 3]));
R = gather(isapprox(A, single([1 2.5 3])));
asserts.isequal(R, [true false true]);
R = gather(isapprox(A, [1 2.5 3], 'AbsoluteTolerance', 0.6));
asserts.isequal(R, [true true true]);
R = gather(isapprox(A, single(1 + 1e-5), 'loose'));
asserts.isequal(R, [true false false]);
%=============================================================================
% isapproxnorm and assert_isapprox reach the gpuArray fallback
asserts.istrue(isapproxnorm(A, single([1 2 3])));
asserts.isfalse(isapproxnorm(A, single([1 2.5 3]), 1e-3));
assert_isapprox(A, single([1 2 3]));
asserts.isfalse(assert_isapprox(A, single([1 2.5 3]), 1e-3));
%=============================================================================
