%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% NaN and Inf are factored like any value and spread through the factors.
%=============================================================================
[L, U, p] = lu([2 0 1; NaN 1 1; 1 1 1], 'vector');
asserts.isequal(p, [1 2 3]);
asserts.istrue(isequaln(L, [1 0 0; NaN 1 0; 0.5 NaN 1]));
asserts.istrue(isequaln(U, [2 0 1; 0 NaN NaN; 0 0 NaN]));
asserts.istrue(isequaln(lu([2 0 1; NaN 1 1; 1 1 1]), [2 0 1; NaN NaN NaN; 0.5 NaN NaN]));
%=============================================================================
% a NaN is a pivot only when it is the first candidate
[L, U, P] = lu([1 2; NaN 3]);
asserts.isequal(P, eye(2));
asserts.istrue(isequaln(L, [1 0; NaN 1]));
asserts.istrue(isequaln(U, [1 2; 0 NaN]));
[L, U, P] = lu([NaN 2; 1 3]);
asserts.isequal(P, eye(2));
asserts.istrue(isequaln(U, [NaN 2; 0 NaN]));
%=============================================================================
% Inf pivots
[L, U] = lu([1 2; Inf 3]);
asserts.isequal(L, [0 1; 1 0]);
asserts.isequal(U, [Inf 3; 0 2]);
[L, U] = lu([Inf 1; Inf 2]);
asserts.istrue(isequaln(L, [1 0; NaN 1]));
asserts.istrue(isequaln(U, [Inf 1; 0 NaN]));
[L, U, P] = lu([1 Inf; 2 3]);
asserts.isequal(L, [1 0; 0.5 1]);
asserts.isequal(U, [2 3; 0 Inf]);
asserts.isequal(P, [0 1; 1 0]);
[L, U] = lu([Inf -Inf; 1 2]);
asserts.isequal(L, eye(2));
asserts.istrue(isequaln(U, [Inf -Inf; 0 NaN]));
%=============================================================================
% rectangular, single and complex
[L, U, p] = lu([1 NaN; 2 3; 4 5], 'vector');
asserts.isequal(p, [3 2 1]);
asserts.istrue(isequaln(L, [1 0; 0.5 1; 0.25 NaN]));
asserts.isequal(U, [4 5; 0 0.5]);
[L, U] = lu(single([1 NaN; 2 3]));
asserts.isequal(class(U), 'single');
asserts.isequal(L, single([0.5 1; 1 0]));
asserts.istrue(isequaln(U, single([2 3; 0 NaN])));
[L, U] = lu([1+1i NaN; 2 3]);
asserts.isequal(L, [1 0; 1-1i 1]);
asserts.istrue(isnan(real(U(2, 2))) && isnan(imag(U(2, 2))));
[L, U] = lu([Inf+1i 1; 2 3]);
asserts.isequal(L, complex(eye(2)));
asserts.isequal(U, [Inf+1i 1; 0 3]);
%=============================================================================
% zero pivot next to a NaN
[L, U] = lu([0 0; 0 NaN]);
asserts.isequal(L, eye(2));
asserts.istrue(isequaln(U, [0 0; 0 NaN]));
%=============================================================================
% larger matrices go through LAPACK and accept NaN and Inf too
rng(4);
B = rand(30);
B(5, 7) = NaN;
B(20, 3) = Inf;
[L, U, p] = lu(B, 'vector');
asserts.isequal(size(L), [30 30]);
asserts.istrue(any(isnan(U(:))));
%=============================================================================
