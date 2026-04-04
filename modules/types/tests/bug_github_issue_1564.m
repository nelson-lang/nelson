%=============================================================================
% Copyright (c) 2026 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/1564
% <-- Short Description -->
% Regression: modifying copy unexpectedly alters original array
%=============================================================================
A = [1 2];
B = [1 2];
mat = A;
mat(1)=0;
assert_isequal(A, B);
assert_isequal(mat, [0 2]);
%=============================================================================
A = [1 2];
mat = A;
mat(1,1)=0;
assert_isequal(A, B);
assert_isequal(mat, [0 2]);
%=============================================================================
A = [1 2];
mat = A;
mat(1,1,1)=0;
assert_isequal(A, B);
assert_isequal(mat, [0 2]);
%=============================================================================
