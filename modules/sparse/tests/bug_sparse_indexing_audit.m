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
% sparse linear indexing: bounds checks, growth semantics, complex and
% logical values (2026-07 audit fixes).
%=============================================================================
% out-of-bounds and invalid linear indices must error (used to crash)
S = sparse(eye(3));
asserts.checkerror('S(0)', _('index must either be real positive integers or logicals.'));
asserts.checkerror('S(10)', _('Index exceeds array bounds.'));
asserts.checkerror('S(2.5)', _('index must either be real positive integers or logicals.'));
asserts.checkerror('S(15) = 5;', _('In an assignment A(I) = B, a matrix A cannot be resized.'));
%=============================================================================
% S(true) selects the first element
S = sparse([7 8; 9 10]);
asserts.isequal(full(S(true)), 7);
%=============================================================================
% sparse vectors grow through linear indexed assignment
V = sparse([1 2]);
V(5) = 3;
asserts.isequal(size(V), [1 5]);
asserts.isequal(full(V), [1 2 0 0 3]);
W = sparse([1; 2]);
W(4) = 7;
asserts.isequal(size(W), [4 1]);
asserts.isequal(full(W), [1; 2; 0; 7]);
%=============================================================================
% complex sparse extraction keeps values with a zero real or imaginary part
S = sparse([3 + 4i, 5 + 0i, 2i]);
R = full(S([2 3 1]));
asserts.isequal(R, [5, 2i, 3 + 4i]);
%=============================================================================
% logical sparse vector assignment writes every element
L = sparse(logical(zeros(2, 2)));
L([1 4]) = [true true];
asserts.isequal(full(L), logical([1 0; 0 1]));
%=============================================================================
% sparse(I, J, V, m, n) must reject indices outside [m, n]
asserts.checkerror('sparse(4, 1, 1, 2, 8)', _('Index exceeds array bounds.'));
asserts.checkerror('sparse(1, 5, 1, 2, 3)', _('Index exceeds array bounds.'));
%=============================================================================
% element-wise access helpers use the same 0-based convention as dense
c = num2cell(sparse([4 5 6]));
asserts.isequal(full(c{1}), 4);
asserts.isequal(full(c{3}), 6);
%=============================================================================
