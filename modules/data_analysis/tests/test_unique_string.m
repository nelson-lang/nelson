%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = ["one","two";"twenty-two","One";"two", "one"];
R = unique(A);
REF = ["One";   "one";   "twenty-two";   "two"];
assert_isequal(R, REF);
%=============================================================================
A = ["one","two";"twenty-two","One";"two", "one"];
[R, a] = unique(A);
REF = ["One";   "one";   "twenty-two";   "two"];
a_REF = [5; 1; 2; 3];
assert_isequal(R, REF);
assert_isequal(a, a_REF);
%=============================================================================
A = ["one","two";"twenty-two","One";"two", "one"];
[R, a, b] = unique(A);
REF = ["One";   "one";   "twenty-two";   "two"];
a_REF = [5; 1; 2; 3];
b_REF = [2; 3; 4; 4; 1; 2];
assert_isequal(R, REF);
assert_isequal(a, a_REF);
assert_isequal(b, b_REF);
%=============================================================================
A = ["one","two";"twenty-two","One";"two", "one"];
R = unique(A, 'rows');
REF = ["one", "two"; "twenty-two", "One";"two", "one"];
assert_isequal(R, REF);
%=============================================================================
A = ["one","two";"twenty-two","One";"two", "one"];
[R,a] = unique(A, 'rows');
REF = ["one", "two"; "twenty-two", "One";"two", "one"];
a_REF = [1; 2; 3];
assert_isequal(R, REF);
assert_isequal(a, a_REF);
%=============================================================================
A = ["one","two";"twenty-two","One";"two", "one"];
[R, a, b] = unique(A, 'rows');
a_REF = [1; 2; 3];
b_REF = [1; 2; 3];
assert_isequal(R, REF);
assert_isequal(a, a_REF);
assert_isequal(b, b_REF);
%=============================================================================
A = ["one","two","twenty-two","One","two"];
R = unique(A);
REF = ["One",   "one",   "twenty-two",   "two"];
assert_isequal(R, REF);
%=============================================================================
A = ["one","two","twenty-two","One","two"];
R = unique(A);
REF = ["One",   "one",   "twenty-two",   "two"];
assert_isequal(R, REF);
%=============================================================================
A = ["one","two";"twenty-two","One";"two", "one"];
R = unique(A, 'rows');
REF = ["one", "two";
    "twenty-two", "One";
    "two", "one"];
assert_isequal(R, REF);
%=============================================================================
[C, ia, ic] = unique(["z"; "z"; "z"]);
C_REF = "z";
assert_isequal(C, C_REF);
ia_REF = 1;
assert_isequal(ia, ia_REF);
ic_REF = [1;1;1];
assert_isequal(ic, ic_REF);
%=============================================================================
[C, ia] = unique(["z"; "z"; "z"]);
C_REF = "z";
assert_isequal(C, C_REF);
ia_REF = 1;
assert_isequal(ia, ia_REF);
%=============================================================================
[C] = unique(["z"; "z"; "z"]);
C_REF = "z";
assert_isequal(C, C_REF);
%=============================================================================
