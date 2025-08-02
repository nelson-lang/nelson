%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = uint8([0 1; 0 1]);
B = uint8([0 0; 1 1]);
R = bitand(A, B);
REF = uint8([ 0   0; 0   1]);
assert_isequal(R, REF);
%=============================================================================
assert_isequal(bitand(5, 3), 1);
assert_isequal(bitand(5, 3, 'int8'), 1);
assert_isequal(bitand(uint8(5), 3), uint8(1));
assert_isequal(bitand(5, uint8(3)), uint8(1));
assert_isequal(bitand(1, logical(1)), 1);
assert_isequal(bitand([], 3), []);
%=============================================================================
