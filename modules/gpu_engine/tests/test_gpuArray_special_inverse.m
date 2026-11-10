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
% digamma, polygamma and exponential integral stay on the device
x = [0.5 1 2 10 100];
R = psi(gpuArray(x));
assert_istrue(isgpuarray(R));
assert_isapprox(gather(R), single(psi(x)), 1e-5);
assert_isapprox(gather(psi(2, gpuArray(x))), single(psi(2, x)), 1e-5);
R = expint(gpuArray(x));
assert_istrue(isgpuarray(R));
assert_isapprox(gather(R), single(expint(x)), 1e-4);
R = expint(gpuArray([-1 2]));
assert_istrue(isgpuarray(R));
assert_isapprox(gather(R), single(expint([-1 2])), 1e-5);
%=============================================================================
% Legendre functions and inverse incomplete gamma and beta functions
R = legendre(3, gpuArray([0.1 0.7]), 'norm');
assert_istrue(isgpuarray(R));
assert_isapprox(gather(R), single(legendre(3, [0.1 0.7], 'norm')), 1e-5);
R = gammaincinv(gpuArray([0.1 0.5 0.9]), 2);
assert_istrue(isgpuarray(R));
assert_isapprox(gather(R), single(gammaincinv([0.1 0.5 0.9], 2)), 1e-5);
R = betaincinv(gpuArray([0.1 0.5 0.9]), 2, 3, 'upper');
assert_istrue(isgpuarray(R));
assert_isapprox(gather(R), single(betaincinv([0.1 0.5 0.9], 2, 3, 'upper')), 1e-5);
%=============================================================================
