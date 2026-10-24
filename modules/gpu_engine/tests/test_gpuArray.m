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
% round trip host -> device -> host (single)
A = single([1 2 3; 4 5 6]);
G = gpuArray(A);
assert_istrue(isgpuarray(G));
assert_isequal(class(G), 'gpuArray');
assert_isequal(underlyingType(G), 'single');
assert_istrue(isUnderlyingType(G, 'single'));
assert_isfalse(isUnderlyingType(G, 'double'));
assert_isequal(size(G), [2 3]);
assert_isequal(numel(G), 6);
assert_isequal(ndims(G), 2);
R = gather(G);
assert_isequal(R, A);
%=============================================================================
% double is uploaded as single (documented downcast, WGSL has no f64)
B = [1.5 2.5; 3.5 4.5];
G2 = gpuArray(B);
assert_isequal(underlyingType(G2), 'single');
assert_isequal(gather(G2), single(B));
%=============================================================================
% gather on a host value is the identity
assert_isequal(gather(42), 42);
%=============================================================================
% isgpuarray on host values
assert_isfalse(isgpuarray(1));
assert_isfalse(isgpuarray('hello'));
%=============================================================================
% unsupported inputs
assert_checkerror('gpuArray(sparse(eye(3)))', _('gpuArray: sparse inputs are not supported.'));
% complex inputs are supported (interleaved re/im on device)
z = gpuArray(single(1 + 2i));
assert_istrue(isgpuarray(z));
assert_isfalse(isreal(z));
assert_isapprox(gather(z), single(1 + 2i), 1e-5);
assert_checkerror('gpuArray(''hello'')', _('gpuArray: numeric or logical input expected.'));
%=============================================================================
