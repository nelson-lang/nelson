%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A scalar right-hand side expands to fill a multi-subscript range even when the
% assignment grows a previously undefined (empty) variable, e.g. A(1:3,1:3) = 7.
clear A
A(1:3, 1:3) = 7;
assert_isequal(A, 7 * ones(3, 3));
%=============================================================================
clear B
B(2:4, 2:4) = 5;
assert_isequal(size(B), [4 4]);
assert_isequal(B(3, 3), 5);
assert_isequal(B(1, 1), 0);
%=============================================================================
clear C
C(1:6, 1:6, 3, 1:1) = 9;
assert_isequal(size(C), [6 6 3]);
assert_isequal(C(4, 4, 3), 9);
assert_isequal(C(4, 4, 1), 0);
%=============================================================================
% A non-scalar right-hand side whose size does not match the range still errors.
assert_checkerror('clear D; D(1:3, 1:3) = [1 2 3 4];', _('Size mismatch in assignment A(I1,I2,...,In) = B.'));
%=============================================================================
% Growing an existing array with a scalar keeps working.
E = 1;
E(1:3, 1:3) = 8;
assert_isequal(E(1, 1), 8);
assert_isequal(size(E), [3 3]);
%=============================================================================
