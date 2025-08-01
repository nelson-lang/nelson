%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('isnumeric'), 1);
assert_isequal(nargout('isnumeric'), 1);
%=============================================================================
A = 1;
res = isnumeric(A);
assert_istrue(res);
%=============================================================================
B = single(1+i);
res = isnumeric(B);
assert_istrue(res);
%=============================================================================
C = logical(1);
res = isnumeric(C);
assert_isfalse(res);
%=============================================================================
A = int8(3);
res = isnumeric(A);
assert_istrue(res);
%=============================================================================
A = int16(3);
res = isnumeric(A);
assert_istrue(res);
%=============================================================================
A = int32(3);
res = isnumeric(A);
assert_istrue(res);
%=============================================================================
A = int64(3);
res = isnumeric(A);
assert_istrue(res);
%=============================================================================
A = uint8(3);
res = isnumeric(A);
assert_istrue(res);
%=============================================================================
A = uint16(3);
res = isnumeric(A);
assert_istrue(res);
%=============================================================================
A = uint32(3);
res = isnumeric(A);
assert_istrue(res);
%=============================================================================
A = uint64(3);
res = isnumeric(A);
assert_istrue(res);
%=============================================================================
A = sparse(3);
res = isnumeric(A);
assert_istrue(res);
%=============================================================================
A = sparse(true);
res = isnumeric(A);
assert_isfalse(res);
%=============================================================================
A = 'Nelson';
res = isnumeric(A);
assert_isfalse(res);
%=============================================================================










A = 3;
res = isinteger(A);
assert_isfalse(res);
%=============================================================================
A = single([3, i]);
res = isinteger(A);
assert_isfalse(res);
%=============================================================================
B = int8(3);
res = isinteger(B);
assert_istrue(res);
%=============================================================================
B = int16(3);
res = isinteger(B);
assert_istrue(res);
%=============================================================================
B = int32(3);
res = isinteger(B);
assert_istrue(res);
%=============================================================================
B = int64(3);
res = isinteger(B);
assert_istrue(res);
%=============================================================================
B = uint8(3);
res = isinteger(B);
assert_istrue(res);
%=============================================================================
B = uint16(3);
res = isinteger(B);
assert_istrue(res);
%=============================================================================
B = uint32(3);
res = isinteger(B);
assert_istrue(res);
%=============================================================================
B = uint64(3);
res = isinteger(B);
assert_istrue(res);
%=============================================================================
