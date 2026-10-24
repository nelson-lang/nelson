%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Short Description -->
% indexing, assignment and deletion on sparse single arrays (real and complex)
% return sparse single results.
%=============================================================================
x = [single(1), sparse(1)];
R = x(:);
asserts.istrue(issparse(R));
asserts.isequal(full(R), single([1; 1]));
R = x([2 1 2]);
asserts.istrue(issparse(R));
asserts.isequal(full(R), single([1 1 1]));
%=============================================================================
m = sparse(single([1 0 2; 0 3 0]));
R = m(:);
asserts.istrue(issparse(R));
asserts.isequal(full(R), single([1; 0; 0; 3; 2; 0]));
asserts.isequal(nnz(R), 3);
R = m([1 6; 2 3]);
asserts.istrue(issparse(R));
asserts.isequal(full(R), single([1 0; 0 0]));
R = m(2, :);
asserts.istrue(issparse(R));
asserts.isequal(full(R), single([0 3 0]));
R = m(:, [3 1]);
asserts.istrue(issparse(R));
asserts.isequal(full(R), single([2 1; 0 0]));
R = m(logical([1 0 1; 0 1 0]));
asserts.istrue(issparse(R));
asserts.isequal(full(R), single([1; 3; 2]));
R = m(m > 1);
asserts.istrue(issparse(R));
asserts.isequal(full(R), single([3; 2]));
R = m(:, 2)';
asserts.istrue(issparse(R));
asserts.isequal(full(R), single([0 3]));
%=============================================================================
c = sparse(single([1+2i 0 3; 0 0 -1i]));
R = c(:);
asserts.istrue(issparse(R));
asserts.isequal(full(R), single([1+2i; 0; 0; 0; 3; -1i]));
R = c(3);
asserts.istrue(issparse(R));
asserts.istrue(isreal(R));
asserts.isequal(full(R), single(0));
R = c(1, :);
asserts.istrue(issparse(R));
asserts.isequal(full(R), single([1+2i 0 3]));
R = c(:, 1);
asserts.istrue(issparse(R));
asserts.isequal(full(R), single([1+2i; 0]));
R = c(logical([1 0 1; 0 0 1]));
asserts.istrue(issparse(R));
asserts.isequal(full(R), single([1+2i; 3; -1i]));
R = c(:, 3);
asserts.istrue(issparse(R));
asserts.isequal(full(R), single([3; -1i]));
R = c(1, 3);
asserts.istrue(issparse(R));
asserts.istrue(isreal(R));
asserts.isequal(full(R), single(3));
%=============================================================================
a = m; a(2) = 5;
asserts.istrue(issparse(a));
asserts.isequal(full(a), single([1 0 2; 5 3 0]));
a = m; a(1, 3) = 0;
asserts.istrue(issparse(a));
asserts.isequal(full(a), single([1 0 0; 0 3 0]));
a = m; a(:, 2) = [7; 8];
asserts.istrue(issparse(a));
asserts.isequal(full(a), single([1 7 2; 0 8 0]));
a = x; a(4) = 2;
asserts.istrue(issparse(a));
asserts.isequal(full(a), single([1 1 0 2]));
a = m; a(2, 5) = 9;
asserts.istrue(issparse(a));
asserts.isequal(full(a), single([1 0 2 0 0; 0 3 0 0 9]));
a = m; a(logical([1 0 0; 0 1 0])) = 0;
asserts.istrue(issparse(a));
asserts.isequal(full(a), single([0 0 2; 0 0 0]));
a = m; a(1) = 2.5;
asserts.istrue(issparse(a));
asserts.isequal(full(a), single([2.5 0 2; 0 3 0]));
a = m; a(1) = true;
asserts.istrue(issparse(a));
asserts.isequal(full(a), single([1 0 2; 0 3 0]));
a = m; a(1, 1) = sparse(4);
asserts.istrue(issparse(a));
asserts.isequal(full(a), single([4 0 2; 0 3 0]));
a = m; a(:) = single(1:6);
asserts.istrue(issparse(a));
asserts.isequal(full(a), single([1 3 5; 2 4 6]));
a = m; a([1 2]) = sparse(single([4 5]));
asserts.istrue(issparse(a));
asserts.isequal(full(a), single([4 0 2; 5 3 0]));
a = m; a(2, [1 3]) = single([4 5]);
asserts.istrue(issparse(a));
asserts.isequal(full(a), single([1 0 2; 4 3 5]));
a = m; a(2, 2) = NaN;
asserts.istrue(issparse(a));
asserts.isequal(full(a), single([1 0 2; 0 NaN 0]));
a = sparse(single(zeros(2, 0))); a(3) = 1;
asserts.istrue(issparse(a));
asserts.isequal(full(a), single([0 0 1]));
asserts.checkerror('a = m; a(7) = 1;', _('In an assignment A(I) = B, a matrix A cannot be resized.'));
%=============================================================================
a = m; a(1) = 1i;
asserts.istrue(issparse(a));
asserts.isequal(full(a), single([1i 0 2; 0 3 0]));
a = c; a(2) = 4;
asserts.istrue(issparse(a));
asserts.isequal(full(a), single([1+2i 0 3; 4 0 -1i]));
a = c; a(1, :) = 0;
asserts.istrue(issparse(a));
asserts.isequal(full(a), single(complex([0 0 0; 0 0 -1i])));
%=============================================================================
a = m; a(2) = [];
asserts.istrue(issparse(a));
asserts.isequal(full(a), single([1 0 3 2 0]));
a = m; a(:, 2) = [];
asserts.istrue(issparse(a));
asserts.isequal(full(a), single([1 2; 0 0]));
a = m; a(1, :) = [];
asserts.istrue(issparse(a));
asserts.isequal(full(a), single([0 3 0]));
a = x; a(1) = [];
asserts.istrue(issparse(a));
asserts.isequal(full(a), single(1));
a = c; a(:, 2) = [];
asserts.istrue(issparse(a));
asserts.isequal(full(a), single([1+2i 3; 0 -1i]));
%=============================================================================
