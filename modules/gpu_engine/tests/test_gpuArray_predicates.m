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
% shape predicates derived from size() (no host transfer)
v = gpuArray(single(1:5));
c = gpuArray(single((1:5)'));
m = gpuArray(single(magic(4)));
s = gpuArray(single(7));
e = gpuArray(single([]));
assert_istrue(isvector(v));
assert_istrue(isvector(c));
assert_isfalse(isvector(m));
assert_istrue(isscalar(s));
assert_isfalse(isscalar(v));
assert_istrue(ismatrix(m));
assert_istrue(isrow(v));
assert_isfalse(isrow(c));
assert_istrue(iscolumn(c));
assert_isfalse(iscolumn(v));
assert_istrue(isempty(e));
assert_isfalse(isempty(v));
assert_isequal(length(m), 4);
assert_isequal(length(v), 5);
assert_isequal(length(e), 0);
%=============================================================================
% class predicates derived from underlyingType()
assert_istrue(isfloat(v));
assert_isfalse(isfloat(gpuArray(int8([1 2 3]))));
assert_istrue(isinteger(gpuArray(int16([1 2 3]))));
assert_isfalse(isinteger(v));
assert_istrue(islogical(gpuArray(true)));
assert_isfalse(islogical(v));
assert_istrue(isnumeric(v));
assert_istrue(isnumeric(gpuArray(int8(3))));
assert_isfalse(isnumeric(gpuArray(true)));
%=============================================================================
% nnz counts nonzero elements on the device, returns a host double
n = nnz(gpuArray(single([0 1 0; 2 0 3])));
assert_isequal(n, 3);
assert_isequal(class(n), 'double');
%=============================================================================
