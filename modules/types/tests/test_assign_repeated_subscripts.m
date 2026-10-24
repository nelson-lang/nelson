%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A(I, J) = B with a repeated subscript: each occurrence takes its value,
% the last one is kept
A = [1; 2];
A([1 2 1], :) = [10; 20; 30];
assert_isequal(A, [30; 20]);
A = [1; 2];
A([1 2 1], 1) = [10; 20; 30];
assert_isequal(A, [30; 20]);
A = [1 5; 2 6];
A([1 2 1], :) = [10 1; 20 2; 30 3];
assert_isequal(A, [30 3; 20 2]);
A = [1 5; 2 6];
A(:, [2 2]) = [7 8; 9 10];
assert_isequal(A, [1 8; 2 10]);
A = {1; 2};
A([1 2 1], :) = {10; 20; 30};
assert_isequal(A, {30; 20});
A = ["a"; "b"];
A([2 2], 1) = ["c"; "d"];
assert_isequal(A, ["a"; "d"]);
%=============================================================================
% growing with a repeated subscript
A = [1; 2];
A([1 1 4], :) = [5; 6; 7];
assert_isequal(A, [6; 2; 0; 7]);
A = [];
A([1 1], 1) = [3; 4];
assert_isequal(A, 4);
%=============================================================================
% the value must still match the subscripts
assert_checkerror('A = [1; 2]; A([1 2], :) = [1; 2; 3];', _('Size mismatch in assignment A(I1,I2,...,In) = B.'));
assert_checkerror('A = []; A(1:2, 1) = [1; 2; 3];', _('Size mismatch in assignment A(I1,I2,...,In) = B.'));
%=============================================================================
