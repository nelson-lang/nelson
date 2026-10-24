%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% A(idx) with a real numeric index array: single-pass gather (convert, bound
% check, copy). Shapes, classes and error identifiers must match the generic
% ordinal-index path.
%=============================================================================
A = reshape(1:12, 3, 4);
asserts.isequal(A([3 2 1]), [3 2 1]);
asserts.isequal(A([3; 2; 1]), [3; 2; 1]);
asserts.isequal(A([12 1; 5 5]), [12 1; 5 5]);
R = A(reshape(12:-1:1, 2, 3, 2));
asserts.isequal(size(R), [2 3 2]);
asserts.isequal(R(:)', 12:-1:1);
asserts.isequal(A(A), A);
asserts.isequal(A(A + 0), A);
%=============================================================================
% vector sources keep their orientation for a vector index
r = 1:5;
asserts.isequal(r([2; 3]), [2 3]);
asserts.isequal(r([2 3; 4 5]), [2 3; 4 5]);
c = (1:5)';
asserts.isequal(c([2 3]), [2; 3]);
asserts.isequal(c([2 3; 4 5]), [2 3; 4 5]);
x = 5;
asserts.isequal(x([1 1 1]), [5 5 5]);
asserts.isequal(x([1; 1]), [5; 5]);
%=============================================================================
% empty indexes
asserts.isequal(size(A([])), [0 0]);
asserts.isequal(size(A(zeros(1, 0))), [1 0]);
asserts.isequal(size(A(zeros(0, 1))), [0 1]);
asserts.isequal(size(A(ones(0, 3))), [0 3]);
asserts.isequal(size(r(zeros(0, 1))), [1 0]);
asserts.isequal(size(c(zeros(1, 0))), [0 1]);
%=============================================================================
% index classes
asserts.isequal(A(int32([12 1 7])), [12 1 7]);
asserts.isequal(A(uint8([2; 4])), [2; 4]);
asserts.isequal(A(int64([5 6])), [5 6]);
asserts.isequal(A(uint64([11 10])), [11 10]);
asserts.isequal(A(single([3 9])), [3 9]);
asserts.isequal(A(int16([1 1])), [1 1]);
%=============================================================================
% source classes
R = int16([7 8 9]);
R = R(int32([3 1]));
asserts.isequal(class(R), 'int16');
asserts.isequal(R, int16([9 7]));
R = single([1 2 3]);
asserts.isequal(R(uint16([3 3])), single([3 3]));
asserts.isequal(uint64([1 2 18446744073709551615])([3 1]), uint64([18446744073709551615 1]));
s = 'hello';
asserts.isequal(s([5 4 1]), 'olh');
asserts.isequal(s(uint32([2 5 1])), 'eoh');
L = [true false true];
R = L([3 2]);
asserts.isequal(class(R), 'logical');
asserts.isequal(R, [true false]);
Z = [1 + 2i, 3, 4];
R = Z([2 3]);
asserts.isequal(R, [3 4]);
asserts.istrue(isreal(R));
asserts.isequal(Z([1 3]), [1 + 2i, 4]);
Zs = single([1 + 2i, 3]);
asserts.isequal(Zs([1 1 2]), single([1 + 2i, 1 + 2i, 3]));
C = {1, 'a', 3};
asserts.isequal(C([3 1]), {3, 1});
%=============================================================================
M = rand(128, 256);
idx = floor(rand(128, 256) * numel(M)) + 1;
R = M(idx);
asserts.isequal(size(R), [128 256]);
asserts.isequal(R(1), M(idx(1)));
asserts.isequal(R(end), M(idx(end)));
asserts.isequal(R(77, 99), M(idx(77, 99)));
asserts.isequal(M(int32(idx)), R);
%=============================================================================
% errors: same identifiers and messages as the generic path; the first invalid
% entry decides which one is raised
msg_real = _('index must either be real positive integers or logicals.');
id_real = 'Nelson:types:invalidIndex:indexEitherRealPositiveIntegersLogicals';
msg_zero = _('Zero or negative index encountered.');
id_zero = 'Nelson:types:invalidIndex:zeroNegativeIndexEncountered';
msg_bound = _('Index exceeds array bounds.');
id_bound = 'Nelson:badsubscript';
assert_checkerror('A([1 0.5])', msg_real, id_real);
assert_checkerror('A([2 NaN])', msg_real, id_real);
assert_checkerror('A([2 Inf])', msg_real, id_real);
assert_checkerror('A(single([1 2.5]))', msg_real, id_real);
assert_checkerror('A([13 0.5])', msg_real, id_real);
assert_checkerror('A([2 0])', msg_zero, id_zero);
assert_checkerror('A([13 0])', msg_zero, id_zero);
assert_checkerror('A(int32([2 -3]))', msg_zero, id_zero);
assert_checkerror('A(uint8([0 2]))', msg_zero, id_zero);
assert_checkerror('A([2 13])', msg_bound, id_bound);
assert_checkerror('A(int8([2 13]))', msg_bound, id_bound);
assert_checkerror('A(uint64([2 18446744073709551615]))', msg_bound, id_bound);
assert_checkerror('x([1 2])', msg_bound, id_bound);
%=============================================================================
