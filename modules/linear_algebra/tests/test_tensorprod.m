%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Expected values are listed explicitly below.
%=============================================================================
A = [1 2; 3 4];
B = [5 6; 7 8];
%=============================================================================
% the outer product has size [size(A) size(B)]
C = tensorprod(A, B);
assert_isequal(size(C), [2 2 2 2]);
assert_isequal(C(:, :, 1, 1), A * 5);
assert_isequal(C(:, :, 2, 2), A * 8);
%=============================================================================
% contracting A dimension 2 with B dimension 1 is the matrix product
assert_isequal(tensorprod(A, B, 2, 1), A * B);
%=============================================================================
% contracting A dimension 1 with B dimension 1 is A.'*B
assert_isequal(tensorprod(A, B, 1, 1), A.' * B);
%=============================================================================
% contracting all dimensions is the full inner product
assert_isequal(tensorprod(A, B, [1 2], [1 2]), sum(A(:) .* B(:)));
assert_isequal(tensorprod(A, B, 'all'), sum(A(:) .* B(:)));
%=============================================================================
% a matrix times a vector keeps a column result
R = tensorprod(A, [1; 1], 2, 1);
assert_isequal(size(R), [2 1]);
assert_isequal(R, [3; 7]);
%=============================================================================
% two vectors contract to a scalar
assert_isequal(tensorprod([1 2 3], [4; 5; 6], 2, 1), 32);
%=============================================================================
% the outer product of two vectors
assert_isequal(size(tensorprod([1; 2; 3], [4 5])), [3 1 1 2]);
%=============================================================================
% contracting one dimension of a 3-D array with a matrix
X = reshape(1:24, 2, 3, 4);
Y = reshape(1:12, 4, 3);
Z = tensorprod(X, Y, 3, 1);
assert_isequal(size(Z), [2 3 3]);
assert_isequal(Z(:, :, 1), [130 150 170; 140 160 180]);
%=============================================================================
% contracting two dimensions of two 3-D arrays
P = reshape(1:24, 2, 3, 4);
Q = reshape(1:24, 3, 4, 2);
R = tensorprod(P, Q, [2 3], [1 2]);
assert_isequal(size(R), [2 2]);
assert_isequal(R, [1222 2950; 1300 3172]);
%=============================================================================
% NumDimensionsA treats trailing singleton dimensions explicitly
assert_isequal(size(tensorprod(A, [10 20], 'NumDimensionsA', 3)), [2 2 1 1 2]);
%=============================================================================
% the class of the inputs is preserved
assert_isequal(class(tensorprod(single([1 2]), [3; 4], 2, 1)), 'single');
%=============================================================================
% mismatched contracted sizes raise an error
assert_checkerror('tensorprod([1 2; 3 4], [1 2 3], 2, 1)', _('Specified dimensions of the input arrays must have the same sizes.'));
%=============================================================================
