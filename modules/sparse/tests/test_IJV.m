%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
S = sparse(eye(3,4));
[A, B, C, D, E] = IJV(S);
REF_A = [1;2;3];
assert_isequal(A, REF_A);
REF_B = [1;2;3];
assert_isequal(B, REF_B);
REF_C = [1;1;1];
assert_isequal(C, REF_C);
REF_D = 3;
assert_isequal(D, REF_D);
REF_E = 4;
assert_isequal(E, REF_E);
%=============================================================================
[A, B, C, D, E] = IJV(sparse([]));
REF_A = zeros(0, 1);
assert_isequal(A, REF_A);
REF_B = zeros(0, 1);
assert_isequal(B, REF_B);
REF_C = zeros(0, 1);
assert_isequal(C, REF_C);
REF_D = 0;
assert_isequal(D, REF_D);
REF_E = 0;
assert_isequal(E, REF_E);
%=============================================================================
[A, B, C, D, E] = IJV(sparse(complex([])));
REF_A = zeros(0, 1);
assert_isequal(A, REF_A);
REF_B = zeros(0, 1);
assert_isequal(B, REF_B);
REF_C = complex(zeros(0, 1));
assert_isequal(C, REF_C);
REF_D = 0;
assert_isequal(D, REF_D);
REF_E = 0;
assert_isequal(E, REF_E);
%=============================================================================
