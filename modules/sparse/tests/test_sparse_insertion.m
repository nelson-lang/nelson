%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = sparse(eye(4,5));
A([5]) = 3;
A([3]) = 3;
A([1]) = 3;
[a, b, c, d, e] = IJV(A);
ref_a = [1; 3; 1; 2; 3; 4];
ref_b = [1; 1; 2; 2; 3; 4];
ref_c = [3; 3; 3; 1; 1; 1];
ref_d = 4;
ref_e = 5;
assert_isequal(a, ref_a);
assert_isequal(b, ref_b);
assert_isequal(c, ref_c);
assert_isequal(d, ref_d);
assert_isequal(e, ref_e);
%=============================================================================
A = sparse(eye(4,5));
A([1,3,1],[2,1]) = 3;
[a, b, c, d, e] = IJV(A);
ref_a = [1; 3; 1; 2; 3; 3; 4];
ref_b = [1; 1; 2; 2; 2; 3; 4];
ref_c = [3; 3; 3; 1; 3; 1; 1];
ref_d = 4;
ref_e = 5;
assert_isequal(a, ref_a);
assert_isequal(b, ref_b);
assert_isequal(c, ref_c);
assert_isequal(d, ref_d);
assert_isequal(e, ref_e);
%=============================================================================
A = sparse(eye(4,5));
A([2,1],[1,3,1]) = 3;
[a, b, c, d, e] = IJV(A);
ref_a = [1; 2; 2; 1; 2; 3; 4];
ref_b = [1; 1; 2; 3; 3; 3; 4];
ref_c = [3; 3; 1; 3; 3; 1; 1];
ref_d = 4;
ref_e = 5;
assert_isequal(a, ref_a);
assert_isequal(b, ref_b);
assert_isequal(c, ref_c);
assert_isequal(d, ref_d);
assert_isequal(e, ref_e);
%=============================================================================
A = sparse(eye(4,5));
A([5 3 1]) = 3;
[a, b, c, d, e] = IJV(A);
ref_a = [1; 3; 1; 2; 3; 4];
ref_b = [1; 1; 2; 2; 3; 4];
ref_c = [3; 3; 3; 1; 1; 1];
ref_d = 4;
ref_e = 5;
assert_isequal(a, ref_a);
assert_isequal(b, ref_b);
assert_isequal(c, ref_c);
assert_isequal(d, ref_d);
assert_isequal(e, ref_e);
%=============================================================================
A = sparse(eye(4, 5));
A(2, [5 3 1]) = 3;
[a, b, c, d, e] = IJV(A);
ref_a = [1; 2; 2; 2; 3; 4; 2];
ref_b = [1; 1; 2; 3; 3; 4; 5];
ref_c = [1; 3; 1; 3; 1; 1; 3.000];
ref_d = 4;
ref_e = 5;
assert_isequal(a, ref_a);
assert_isequal(b, ref_b);
assert_isequal(c, ref_c);
assert_isequal(d, ref_d);
assert_isequal(e, ref_e);
%=============================================================================
A = sparse(eye(4, 5));
A([5 3 1],2) = 3;
[a, b, c, d, e] = IJV(A);
ref_a = [1; 1; 2; 3; 5; 3; 4];
ref_b = [1; 2; 2; 2; 2; 3; 4];
ref_c = [1; 3; 1; 3; 3; 1; 1];
ref_d = 5;
ref_e = 5;
assert_isequal(a, ref_a);
assert_isequal(b, ref_b);
assert_isequal(c, ref_c);
assert_isequal(d, ref_d);
assert_isequal(e, ref_e);
%=============================================================================
