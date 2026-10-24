%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = ["one", "two"; "twenty-two", "One"; "two", "one"];
R = unique(A);
REF = ["One"; "one"; "twenty-two"; "two"];
asserts.isequal(R, REF);
%=============================================================================
A = ["one", "two"; "twenty-two", "One"; "two", "one"];
[R, a] = unique(A);
REF = ["One"; "one"; "twenty-two"; "two"];
a_REF = [5; 1; 2; 3];
asserts.isequal(R, REF);
asserts.isequal(a, a_REF);
%=============================================================================
A = ["one", "two"; "twenty-two", "One"; "two", "one"];
[R, a, b] = unique(A);
REF = ["One"; "one"; "twenty-two"; "two"];
a_REF = [5; 1; 2; 3];
b_REF = [2; 3; 4; 4; 1; 2];
asserts.isequal(R, REF);
asserts.isequal(a, a_REF);
asserts.isequal(b, b_REF);
%=============================================================================
A = ["beta" "alpha" "beta"];
[C, ia, ic] = unique(A, 'stable');
asserts.isequal(C, ["beta" "alpha"]);
asserts.isequal(ia, [1; 2]);
asserts.isequal(ic, [1; 2; 1]);
%=============================================================================
A = ["one", "two"; "twenty-two", "One"; "two", "one"];
R = unique(A, 'rows');
REF = ["one", "two"; "twenty-two", "One"; "two", "one"];
asserts.isequal(R, REF);
%=============================================================================
A = ["one", "two"; "twenty-two", "One"; "two", "one"];
[R, a] = unique(A, 'rows');
REF = ["one", "two"; "twenty-two", "One"; "two", "one"];
a_REF = [1; 2; 3];
asserts.isequal(R, REF);
asserts.isequal(a, a_REF);
%=============================================================================
A = ["one", "two"; "twenty-two", "One"; "two", "one"];
[R, a, b] = unique(A, 'rows');
a_REF = [1; 2; 3];
b_REF = [1; 2; 3];
asserts.isequal(R, REF);
asserts.isequal(a, a_REF);
asserts.isequal(b, b_REF);
%=============================================================================
A = ["one", "two", "twenty-two", "One", "two"];
R = unique(A);
REF = ["One", "one", "twenty-two", "two"];
asserts.isequal(R, REF);
%=============================================================================
A = ["one", "two", "twenty-two", "One", "two"];
R = unique(A);
REF = ["One", "one", "twenty-two", "two"];
asserts.isequal(R, REF);
%=============================================================================
A = ["one", "two"; "twenty-two", "One"; "two", "one"];
R = unique(A, 'rows');
REF = ["one", "two";
    "twenty-two", "One";
    "two", "one"];
asserts.isequal(R, REF);
%=============================================================================
[C, ia, ic] = unique(["z"; "z"; "z"]);
C_REF = "z";
asserts.isequal(C, C_REF);
ia_REF = 1;
asserts.isequal(ia, ia_REF);
ic_REF = [1; 1; 1];
asserts.isequal(ic, ic_REF);
%=============================================================================
[C, ia] = unique(["z"; "z"; "z"]);
C_REF = "z";
asserts.isequal(C, C_REF);
ia_REF = 1;
asserts.isequal(ia, ia_REF);
%=============================================================================
[C] = unique(["z"; "z"; "z"]);
C_REF = "z";
asserts.isequal(C, C_REF);
%=============================================================================
