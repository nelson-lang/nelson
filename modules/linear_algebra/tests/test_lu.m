%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('lu'), 1);
assert_isequal(nargout('lu'), 3);
%=============================================================================
A = magic(5);
%=============================================================================
[L, U] = lu(A);
L_REF = [0.7391    1.0000         0         0         0;
    1.0000         0         0         0         0;
    0.1739    0.2527    0.5164    1.0000         0;
    0.4348    0.4839    0.7231    0.9231    1.0000;
    0.4783    0.7687    1.0000         0         0];

U_REF = [23.0000    5.0000    7.0000   14.0000   16.0000;
         0   20.3043   -4.1739   -2.3478    3.1739;
         0         0   24.8608   -2.8908   -1.0921;
         0         0         0   19.6512   18.9793;
         0         0         0         0  -22.2222];

assert_isapprox(L, L_REF, 1e-3);
assert_isapprox(U, U_REF, 1e-3);
%=============================================================================
[L, U, P] = lu(A);

L_REF = [1.0000         0         0         0         0;
    0.7391    1.0000         0         0         0;
    0.4783    0.7687    1.0000         0         0;
    0.1739    0.2527    0.5164    1.0000         0;
    0.4348    0.4839    0.7231    0.9231    1.0000];
U_REF = [23.0000    5.0000    7.0000   14.0000   16.0000;
         0   20.3043   -4.1739   -2.3478    3.1739;
         0         0   24.8608   -2.8908   -1.0921;
         0         0         0   19.6512   18.9793;
         0         0         0         0  -22.2222];

P_REF = [  0     1     0     0     0;
     1     0     0     0     0;
     0     0     0     0     1;
     0     0     1     0     0;
     0     0     0     1     0];
assert_isapprox(L, L_REF, 1e-3);
assert_isapprox(U, U_REF, 1e-3);
assert_isapprox(P, P_REF, 1e-3);
%=============================================================================
B = A + i;
%=============================================================================
[L, U] = lu(B);

L_REF = [0.7396 + 0.0113i   1.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i;
   1.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i;
   0.1755 + 0.0358i   0.2542 + 0.0292i   0.5177 + 0.0204i   1.0000 + 0.0000i   0.0000 + 0.0000i;
   0.4358 + 0.0245i   0.4848 + 0.0169i   0.7235 + 0.0071i   0.9223 - 0.0100i   1.0000 + 0.0000i;
   0.4792 + 0.0226i   0.7694 + 0.0123i   1.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i];

U_REF = [23.0000 + 1.0000i   5.0000 + 1.0000i   7.0000 + 1.0000i  14.0000 + 1.0000i  16.0000 + 1.0000i;
   0.0000 + 0.0000i  20.3132 + 0.2038i  -4.1660 + 0.1811i  -2.3434 + 0.1019i   3.1774 + 0.0792i;
   0.0000 + 0.0000i   0.0000 + 0.0000i  24.8755 + 0.2743i  -2.8825 + 0.1543i  -1.0890 + 0.0583i;
   0.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i  19.6734 + 0.3441i  18.9877 + 0.1300i;
   0.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i -22.2127 + 0.1161i];

assert_isapprox(L, L_REF, 1e-2);
assert_isapprox(U, U_REF, 1e-3);
%=============================================================================
[L, U, P] = lu(B);

L_REF = [1.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i;
   0.7396 + 0.0113i   1.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i;
   0.4792 + 0.0226i   0.7694 + 0.0123i   1.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i;
   0.1755 + 0.0358i   0.2542 + 0.0292i   0.5177 + 0.0204i   1.0000 + 0.0000i   0.0000 + 0.0000i;
   0.4358 + 0.0245i   0.4848 + 0.0169i   0.7235 + 0.0071i   0.9223 - 0.0100i   1.0000 + 0.0000i];
U_REF = [23.0000 + 1.0000i   5.0000 + 1.0000i   7.0000 + 1.0000i  14.0000 + 1.0000i  16.0000 + 1.0000i;
   0.0000 + 0.0000i  20.3132 + 0.2038i  -4.1660 + 0.1811i  -2.3434 + 0.1019i   3.1774 + 0.0792i;
   0.0000 + 0.0000i   0.0000 + 0.0000i  24.8755 + 0.2743i  -2.8825 + 0.1543i  -1.0890 + 0.0583i;
   0.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i  19.6734 + 0.3441i  18.9877 + 0.1300i;
   0.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i -22.2127 + 0.1161i];
P_REF = [0     1     0     0     0;
     1     0     0     0     0;
     0     0     0     0     1;
     0     0     1     0     0;
     0     0     0     1     0];

assert_isapprox(L, L_REF, 1e-2);
assert_isapprox(U, U_REF, 1e-3);
assert_isapprox(P, P_REF, 1e-3);
%=============================================================================
A = single(magic(5));
%=============================================================================
[L, U] = lu(A);
L_REF = [0.7391    1.0000         0         0         0;
    1.0000         0         0         0         0;
    0.1739    0.2527    0.5164    1.0000         0;
    0.4348    0.4839    0.7231    0.9231    1.0000;
    0.4783    0.7687    1.0000         0         0];

U_REF = [23.0000    5.0000    7.0000   14.0000   16.0000;
         0   20.3043   -4.1739   -2.3478    3.1739;
         0         0   24.8608   -2.8908   -1.0921;
         0         0         0   19.6512   18.9793;
         0         0         0         0  -22.2222];

assert_isapprox(L, single(L_REF), 1e-3);
assert_isapprox(U, single(U_REF), 1e-3);
%=============================================================================
[L, U, P] = lu(A);

L_REF = [1.0000         0         0         0         0;
    0.7391    1.0000         0         0         0;
    0.4783    0.7687    1.0000         0         0;
    0.1739    0.2527    0.5164    1.0000         0;
    0.4348    0.4839    0.7231    0.9231    1.0000];
U_REF = [23.0000    5.0000    7.0000   14.0000   16.0000;
         0   20.3043   -4.1739   -2.3478    3.1739;
         0         0   24.8608   -2.8908   -1.0921;
         0         0         0   19.6512   18.9793;
         0         0         0         0  -22.2222];

P_REF = [  0     1     0     0     0;
     1     0     0     0     0;
     0     0     0     0     1;
     0     0     1     0     0;
     0     0     0     1     0];
assert_isapprox(L, single(L_REF), 1e-3);
assert_isapprox(U, single(U_REF), 1e-3);
assert_isapprox(P, single(P_REF), 1e-3);
%=============================================================================
B = single(A + i);
%=============================================================================
[L, U] = lu(B);

L_REF = [0.7396 + 0.0113i   1.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i;
   1.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i;
   0.1755 + 0.0358i   0.2542 + 0.0292i   0.5177 + 0.0204i   1.0000 + 0.0000i   0.0000 + 0.0000i;
   0.4358 + 0.0245i   0.4848 + 0.0169i   0.7235 + 0.0071i   0.9223 - 0.0100i   1.0000 + 0.0000i;
   0.4792 + 0.0226i   0.7694 + 0.0123i   1.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i];

U_REF = [23.0000 + 1.0000i   5.0000 + 1.0000i   7.0000 + 1.0000i  14.0000 + 1.0000i  16.0000 + 1.0000i;
   0.0000 + 0.0000i  20.3132 + 0.2038i  -4.1660 + 0.1811i  -2.3434 + 0.1019i   3.1774 + 0.0792i;
   0.0000 + 0.0000i   0.0000 + 0.0000i  24.8755 + 0.2743i  -2.8825 + 0.1543i  -1.0890 + 0.0583i;
   0.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i  19.6734 + 0.3441i  18.9877 + 0.1300i;
   0.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i -22.2127 + 0.1161i];

assert_isapprox(L, single(L_REF), 1e-2);
assert_isapprox(U, single(U_REF), 1e-3);
%=============================================================================
[L, U, P] = lu(B);

L_REF = [1.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i;
   0.7396 + 0.0113i   1.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i;
   0.4792 + 0.0226i   0.7694 + 0.0123i   1.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i;
   0.1755 + 0.0358i   0.2542 + 0.0292i   0.5177 + 0.0204i   1.0000 + 0.0000i   0.0000 + 0.0000i;
   0.4358 + 0.0245i   0.4848 + 0.0169i   0.7235 + 0.0071i   0.9223 - 0.0100i   1.0000 + 0.0000i];
U_REF = [23.0000 + 1.0000i   5.0000 + 1.0000i   7.0000 + 1.0000i  14.0000 + 1.0000i  16.0000 + 1.0000i;
   0.0000 + 0.0000i  20.3132 + 0.2038i  -4.1660 + 0.1811i  -2.3434 + 0.1019i   3.1774 + 0.0792i;
   0.0000 + 0.0000i   0.0000 + 0.0000i  24.8755 + 0.2743i  -2.8825 + 0.1543i  -1.0890 + 0.0583i;
   0.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i  19.6734 + 0.3441i  18.9877 + 0.1300i;
   0.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i -22.2127 + 0.1161i];
P_REF = [0     1     0     0     0;
     1     0     0     0     0;
     0     0     0     0     1;
     0     0     1     0     0;
     0     0     0     1     0];

assert_isapprox(L, single(L_REF), 1e-2);
assert_isapprox(U, single(U_REF), 1e-3);
assert_isapprox(P, single(P_REF), 1e-3);
%=============================================================================
[L, U] = lu([]);
assert_isequal(L, []);
assert_isequal(U, []);
%=============================================================================
[L, U, P] = lu([]);
assert_isequal(L, []);
assert_isequal(U, []);
assert_isequal(P, []);
%=============================================================================
assert_checkerror('[L, U] = lu([1, NaN])', _('Input to LU must not contain NaN or Inf.'));
%=============================================================================
