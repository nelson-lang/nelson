%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% The (i, j, v) triplet constructor, sparse arithmetic, and element assignment
% drop an all-zero imaginary part and yield a real sparse matrix. The sparse()
% conversion of an already-complex dense matrix keeps its complex attribute.
%=============================================================================
% Triplet constructor drops an all-zero imaginary part.
A = sparse(1, 1, complex(1, 0));
asserts.istrue(isreal(A));
asserts.istrue(issparse(A));
asserts.isequal(class(A), 'double');
asserts.isequal(full(A), 1);
%=============================================================================
A = sparse([1 2], [1 1], complex([1; 2], [0; 0]));
asserts.istrue(isreal(A));
asserts.isequal(full(A), [1; 2]);
%=============================================================================
% Triplet constructor keeps a genuine imaginary part.
A = sparse(1, 1, complex(1, 2));
asserts.isfalse(isreal(A));
%=============================================================================
% Triplet with explicit size.
A = sparse([1 2], [1 1], complex([1; 2], [0; 0]), 3, 1);
asserts.istrue(isreal(A));
asserts.isequal(class(A), 'double');
asserts.isequal(full(A), [1; 2; 0]);
%=============================================================================
% Single precision triplet canonicalizes to single, not double.
A = sparse(1, 1, complex(single(1), single(0)));
asserts.istrue(isreal(A));
asserts.isequal(class(A), 'single');
A = sparse(1, 1, complex(single(1), single(2)));
asserts.isfalse(isreal(A));
asserts.isequal(class(A), 'single');
%=============================================================================
% Sparse arithmetic canonicalizes when the imaginary part vanishes.
asserts.istrue(isreal(complex(1, 0) + sparse(0)));
asserts.istrue(isreal(complex(1, 0) - sparse(0)));
asserts.istrue(isreal(sparse(1, 1, complex(2, 3)) - sparse(1, 1, complex(2, 3))));
asserts.istrue(isreal(sparse(1, 1, complex(1, 2)) + sparse(1, 1, complex(1, -2))));
asserts.istrue(isreal(sparse(1, 1, complex(1, 0)) .* 2));
asserts.istrue(isreal(2 * sparse(1, 1, complex(1, 0))));
asserts.istrue(isreal(sparse(1, 1, complex(1, 2)) .* complex(0, 0)));
%=============================================================================
% Sparse arithmetic keeps a genuine imaginary part.
asserts.isfalse(isreal(sparse(1, 1, complex(1, 2)) + sparse(1, 1, complex(3, 4))));
asserts.isfalse(isreal(sparse(1, 1, complex(1, 2)) .* sparse(1, 1, complex(3, 4))));
%=============================================================================
% Element assignment that removes the last imaginary entry canonicalizes.
A = sparse(1, 1, complex(1, 2));
A(1, 1) = 1;
asserts.istrue(isreal(A));
asserts.isequal(full(A), 1);
%=============================================================================
A = sparse(1, 1, complex(1, 2));
A(1, 1) = complex(3, 0);
asserts.istrue(isreal(A));
%=============================================================================
A = sparse(2, 2);
A(1, 1) = complex(1, 0);
asserts.istrue(isreal(A));
%=============================================================================
% Linear-index assignment also canonicalizes.
A = sparse(1, 1, complex(1, 2));
A(1) = 5;
asserts.istrue(isreal(A));
%=============================================================================
% Element assignment keeps complex while any imaginary entry remains.
A = sparse(2, 1);
A(1, 1) = complex(1, 2);
A(2, 1) = complex(3, 0);
asserts.isfalse(isreal(A));
%=============================================================================
% sparse() of an already-complex dense matrix preserves the complex attribute.
asserts.isfalse(isreal(sparse(complex([1 2 3], 0))));
asserts.isfalse(isreal(sparse(complex(1, 0))));
asserts.isfalse(isreal(sparse(complex(single([1 2 3]), single(0)))));
%=============================================================================
