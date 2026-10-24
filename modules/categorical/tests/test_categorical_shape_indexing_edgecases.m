%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = categorical({'a', 'b', ''; 'c', 'a', 'b'});
asserts.isequal(size(A), [2 3]);
asserts.isequal(size(A, 1), 2);
asserts.isequal(size(A, 2), 3);
[r, c] = size(A);
asserts.isequal([r c], [2 3]);
asserts.isequal(numel(A), 6);
asserts.isequal(length(A), 3);
asserts.isequal(ndims(A), 2);
asserts.istrue(ismatrix(A));
asserts.isfalse(isempty(A));
asserts.isfalse(isrow(A));
asserts.isfalse(iscolumn(A));
asserts.isfalse(isvector(A));
asserts.isfalse(isscalar(A));
%=============================================================================
E = categorical({});
asserts.istrue(isempty(E));
asserts.isequal(size(E), [0 0]);
asserts.isequal(numel(E), 0);
%=============================================================================
row = A(1, :);
asserts.istrue(isrow(row));
asserts.isequal(cellstr(row), {'a', 'b', '<undefined>'});
col = A(:, 2);
asserts.istrue(iscolumn(col));
asserts.isequal(cellstr(col), {'b'; 'a'});
last = A(end);
asserts.isequal(cellstr(last), {'b'});
%=============================================================================
R = reshape(A, 3, 2);
asserts.isequal(size(R), [3 2]);
asserts.isequal(categories(R), categories(A));
P = permute(A, [2 1]);
asserts.isequal(size(P), [3 2]);
asserts.isequal(cellstr(P(1, :)), {'a', 'c'});
T = A.';
CT = A';
asserts.isequal(size(T), [3 2]);
asserts.istrue(isequal(T, CT));
%=============================================================================
B = A;
B(1, 2) = 'd';
asserts.isequal(cellstr(B(1, 2)), {'d'});
asserts.istrue(iscategory(B, 'd'));
B(2, :) = [];
asserts.isequal(size(B), [1 3]);
asserts.isequal(cellstr(B), {'a', 'd', '<undefined>'});
%=============================================================================
C = parenReference(A, 2, 1:2);
asserts.isequal(cellstr(C), {'c', 'a'});
D = parenAssign(A, 'd', 1, 1);
asserts.isequal(cellstr(D(1, 1)), {'d'});
asserts.istrue(iscategory(D, 'd'));
asserts.isequal(numArgumentsFromSubscript(A, struct('type', '()', 'subs', {{1}})), 1);
asserts.isequal(subsindex(categorical({'a', 'b'})), [0 1]);
codes = A.codes;
codes(codes == 0) = NaN;
asserts.isequal(codes, double(A));
asserts.isequal(A.categoryNames, categories(A));
%=============================================================================
asserts.checkerror('A{1}', _('Brace indexing is not supported for categorical arrays.'));
asserts.checkerror('A{1}', _('Brace indexing is not supported for categorical arrays.'), 'Nelson:categorical:braceIndexingNotSupported');
asserts.checkerror('A.invalidProperty', _('Dot indexing is not supported for categorical arrays.'));
asserts.checkerror('A.invalidProperty', _('Dot indexing is not supported for categorical arrays.'), 'Nelson:categorical:dotIndexingNotSupported');
%=============================================================================
