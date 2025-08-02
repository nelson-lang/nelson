%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = sparse([]);
assert_isequal(size(A), [0 0]);
assert_isequal(class(A), 'sparsedouble');
assert_istrue(issparse(A));
A(10, 11) = 1;
assert_isequal(size(A), [10 11]);
[a, b, c, d, e] = IJV(A);
ref_a = 10;
ref_b = 11;
ref_c = 1;
ref_d = 10;
ref_e = 11;
assert_isequal(a, ref_a);
assert_isequal(b, ref_b);
assert_isequal(c, ref_c);
assert_isequal(d, ref_d);
assert_isequal(e, ref_e);
%=============================================================================
a = logical(sparse(1));
assert_isequal(class(a), 'sparselogical');
assert_isequal(size(a), [1 1]);
assert_istrue(full(a(1, 1)));
%=============================================================================
assert_checkerror('S = sparse(zeros(3, 3), 3, 3)', _('in I, J, V format, all three vectors must be the same size or be scalars.'));
%=============================================================================
R = sparse([], 4, 4);
REF = sparse([]);
assert_isequal(R, REF);
%=============================================================================
R = sparse([], 4, 4, 5, 6);
S = size(R);
assert_isequal(S, [5, 6]);
%=============================================================================
