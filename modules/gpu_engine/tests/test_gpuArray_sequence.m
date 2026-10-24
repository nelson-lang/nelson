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
% linspace with a gpuArray endpoint builds a device row vector; the endpoint is
% pinned exactly.
L = linspace(gpuArray(single(0)), 1, 5);
assert_istrue(isgpuarray(L));
assert_isapprox(gather(L), single(linspace(0, 1, 5)), 1e-5);
L2 = gather(linspace(gpuArray(single(0)), 10)); % default 100 points
assert_isequal(numel(L2), 100);
assert_isapprox(L2, single(linspace(0, 10, 100)), 1e-4);
assert_isequal(L2(100), single(10)); % endpoint pinned
assert_isapprox(gather(linspace(gpuArray(single(2)), 3, 1)), single(3), 1e-6);
%=============================================================================
% colon (the : operator) with a gpuArray endpoint runs on the device
C = gpuArray(single(1)):single(5);
assert_istrue(isgpuarray(C));
assert_isequal(gather(C), single(1:5));
assert_isequal(gather(gpuArray(single(1)):2:single(10)), single(1:2:10));
assert_isequal(gather(gpuArray(single(10)):-2:single(1)), single(10:-2:1));
assert_isequal(gather(gpuArray(single(0)):0.25:single(1)), single(0:0.25:1));
%=============================================================================
% an empty range gives an empty result
assert_isequal(numel(gather(gpuArray(single(5)):single(1))), 0);
%=============================================================================
