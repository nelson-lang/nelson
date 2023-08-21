%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_checkerror('isequal()', _('Wrong number of input arguments.'));
assert_checkerror('isequal(1)', _('Wrong number of input arguments.'));
assert_checkerror('isequal([1, 1])', _('Wrong number of input arguments.'));
%=============================================================================
assert_isequal(nargin('isequal'), -1);
assert_isequal(nargout('isequal'), 1);
%=============================================================================
assert_isequal(isequal(struct([]), struct([])), true);
assert_isequal(isequal(struct('nelson', 1), struct('nelson', 1)), true);
assert_isequal(isequal(struct('nelson', 1), struct('nelson', 2)), false);
assert_isequal(isequal(struct('nelson', 1), struct('sonnel', 1)), false);
assert_isequal(isequal(struct('nelson', 1, 'sonnel', 2), struct('nelson', 1, 'sonnel', 2)), true);
assert_isequal(isequal(struct('nelson', 1, 'sonnel', 2), struct('sonnel', 2, 'nelson', 1)), true);
assert_isequal(isequal(struct('nelson', 1, 'sonnel', 2), struct('nelson', 1, 'sonnel', 2), struct ('nelson', 1, 'sonnel', 2)), true);
assert_isequal(isequal(struct('nelson', 'abc', 'sonnel', 2), struct ('nelson', 'abc', 'sonnel', 2)), true);
%=============================================================================
assert_isequal(isequal(11, 11), true);
assert_isequal(isequal([11, 11], [11, 11]), true);
assert_isequal(isequal([11; 11; 11], [11; 11; 11]), true);
assert_isequal(isequal(complex(11, 0), 11), true);
%=============================================================================
assert_isequal(isequal([11, 21, 31, 41], [11, 21, 31, 41]), true);
assert_isequal(isequal([11; 21; 31; 41], [11; 21; 31; 41]), true);
assert_isequal(isequal([11, 21, 31, 41], [11; 21; 31; 41]), false);
assert_isequal(isequal([11, 21, 31, 41], [11, 21; 31, 41]), false);
assert_isequal(isequal([11, 21, 31, 41], [11, 31; 21, 41]), false);
%=============================================================================
assert_isequal(isequal([], []), true);
assert_isequal(isequal([], single([])), true);
%=============================================================================
assert_isequal(isequal([], [], 1), false);
assert_isequal(isequal([], 1, []), false);
assert_isequal(isequal(1, [], []), false);
assert_isequal(isequal(1, [], []), false);
%=============================================================================
assert_isequal(isequal('nelson', 'nelson'), true);
assert_isequal(isequal('nelson', 'sonnel'), false);
assert_isequal(isequal('nelsonsonnelnelsonsonnel', ['nelson', 'sonnel', 'nelson', 'sonnel']), true);
assert_isequal(isequal(['nelson', 'sonnel', 'son', 'nel'], ['nelson', 'sonnel', 'son', 'nel']), true);
assert_isequal(isequal(['nelson', 'sonnel', 'son', 'e'], ['nelson', 'sonnel', 'son', 'nel']), false);
assert_isequal(isequal(['TesT   '; 'sTring '], ['TesT   '; 'sTring '], ['TesT   '; 'sTring ']), true);
assert_isequal(isequal(['nelson', 'sonnel', 'son', 'nel'], ['nelson'; 'sonnel'; 'son   '; 'nel   ']), false);
%=============================================================================
A = str2func('cos');
B = str2func('sin');
assert_isequal(isequal(A, A), true);
assert_isequal(isequal(A, B), false);
%=============================================================================
assert_isequal(isequal(cell(11, 11), cell(11, 11)), true);
assert_isequal(isequal(cell(11, 11), cell(11, 21)), false);
assert_isequal(isequal({'nelson', 1}, {'nelson', 1}), true);
assert_isequal(isequal({'nelson', 1}, {'nelson', 2}), false);
assert_isequal(isequal({'nelson', 1}, {'sonnel', 1}), false);
assert_isequal(isequal({'nelson', 1, 'sonnel', 2}, {'nelson', 1, 'sonnel',2}), true);
assert_isequal(isequal({'nelson', 1, 'sonnel', 2}, {'sonnel', 2, 'nelson', 1}), false);
assert_isequal(isequal({'nelson', 1, 'sonnel', 2}, {'nelson', 1, 'sonnel', 2}, {'nelson', 1, 'sonnel',2}), true);
assert_isequal(isequal({'nelson', 'abc', 'sonnel', 2}, {'nelson', 'abc', 'sonnel', 2}), true);
assert_isequal(isequal({'nelson', 'sonnel', 'son', 'nel'}, {'nelson', 'sonnel', 'son', 'nel'}), true);
assert_isequal(isequal({'nelson', 'sonnel', 'son', 'nel'}, {'nelson'; 'sonnel'; 'son'; 'nel'}), false);
assert_isequal(isequal(['nelson', 'sonnel', 'son', 'nel'], {'nelson', 'sonnel', 'son', 'nel'}), false);
%=============================================================================
assert_isequal(isequal(sparse([]), []), true);
assert_isequal(isequal([], sparse([])), true);
assert_isequal(isequal(sparse([]), sparse([])), true);
assert_isequal(isequal(sparse(0, 1), sparse(0, 1)), true);
assert_isequal(isequal(sparse(0, 1), zeros(0, 1)), true);
assert_isequal(isequal(sparse(2, 2), sparse(2, 2)), true);
assert_isequal(isequal(zeros(2, 2), sparse(2, 2)), true);
assert_isequal(isequal(sparse(0, 1), sparse(1, 0)), false)
%=============================================================================
assert_isequal(isequal(NaN, NaN), false);
assert_isequal(isequal(NaN, Inf), false);
assert_isequal(isequal(NaN, 1.0), false);
assert_isequal(isequal([11, 12, NaN, 14], [11, 12, NaN, 14]), false);
assert_isequal(isequal(struct('nelson', NaN, 'nel', 2), struct('nelson', NaN, 'nel', 2), struct('nelson', NaN, 'nel', 2)), false);
%=============================================================================
assert_isequal(isequal(11, 11, 11, 11, 11), true);
assert_isequal(isequal(11, 11, 41, 21, 11), false);
assert_isequal(isequal(0i+11, 11, 11, 11), true);
assert_isequal(isequal(0i, 0, 0, 0i), true);
assert_isequal(isequal(4i-10, 4i-10), true);
assert_isequal(isequal([11, 41, 21; 51, 11, 21], [11, 41, 21; 51, 11, 21], [11, 41, 21; 51, 11, 21]), true);
assert_isequal(isequal([11, 41, 21; 51, 11, 21], [11, 41, 21; 51 ,11 ,21], [51, 11, 21; 11, 41, 21]), false);
assert_isequal(isequal([21-i, 81, i, 21+i], [21-i, 81, i, 21+i], [21-i, 81, i, 21+i], [21-i, 81, i, 21+i], [21-i, 81, i, 21+i]), true);
assert_isequal(isequal('test_nelson', 'test_nelson', 'test_nelson'), true);
assert_isequal(isequal('test_nelson', 'test_nelson', 'test_nelson2'), false);
assert_isequal(isequal(11, 31, [21, 11]), false);
assert_isequal(isequal(2i, 3i ,41), false);
assert_isequal(isequal([11, 31, 21], [11, 21, 31], 81), false);
assert_isequal(isequal([21], 21), true);
assert_isequal(isequal('test', 31), false);
assert_isequal(isequal(['ab1'; 'cd1'], 'abcd1'), false);
assert_isequal(isequal(['ab1';'cd1'], 'acbd1'), false);
assert_isequal(isequal(['ab1'; 'cd1'], ['a1'; 'b1'; 'c1'; 'd1']), false);
assert_isequal(isequal(['ab1'; 'cd1'], ['a1'; 'c1'; 'b1'; 'd1']), false);
assert_isequal(isequal(['a1b1'; 'c1d1'], ['a1', 'b1'; 'c1', 'd1']), true);
assert_isequal(isequal(['ab1'; 'cd1'], [97, 98, double('1'); 99, 100, double('1')]), true);
assert_isequal(isequal({'test_nelson', ['ab'; 'cd'], 2i-8i, {{9+2i}, [2, 3, 4i]}}, {'test_nelson', ['ab'; 'cd'], 2i-8i, {{9+2i}, [2, 3, 0]}}), false);
assert_isequal(isequal({'test_nelson', ['ab'; 'cd'], 2i-8i, {{9+2i}, [2, 3, 4i]}}, {'test_nelson', ['ab'; 'cd'], 2i-8i, {{7+2i}, [2, 3, 4i]}}), false);
assert_isequal(isequal({'test_nelson', ['ab'; 'cd'], 2i-8i, {{9+2i}, [2, 3, 4i]}}, {'test_nelson', ['ab'; 'cd'], 2i-8i, {{8+2i}, [2, 3, 4i]}}), false);
assert_isequal(isequal({'test_nelson', ['ab'; 'cd'], 2i-8i}, {'test_nelson2', ['ab'; 'cd'], 21i-81i}), false);
assert_isequal(isequal({'test_nelson', ['ab'; 'cd'], 21i-81i}, {'test_nelson',['ab'; 'cd'], 21i-81i}), true);
assert_isequal(isequal({11, 21, 31}, {31; 41; 51}), false);
assert_isequal(isequal({11, 21, 31}, {11, 21, 51}), false);
assert_isequal(isequal({11, 21, 31}, {11, 21, 5i-31}), false);
%=============================================================================
assert_isequal(isequal(false, false), true);
assert_isequal(isequal(false, true), false);
%=============================================================================
assert_isequal(isequal(single(1), double(1)), true);
assert_isequal(isequal(single(1), single(2)), false);
assert_isequal(isequal(single(1), single(1)), true);
assert_isequal(isequal(single(ones(3, 3, 3)), single(ones(3, 3, 3))), true);
%=============================================================================
assert_isequal(isequal(int8(1), double(1)), true);
assert_isequal(isequal(int8(1), int8(1)), true);
assert_isequal(isequal(int8(ones(3, 3, 3)), int8(ones(3, 3, 3))), true);
assert_isequal(isequal(int16(ones(3, 3, 3)), int16(ones(3, 3, 3))), true);
assert_isequal(isequal(int32(ones(3, 3, 3)), int32(ones(3, 3, 3))), true);
assert_isequal(isequal(int64(ones(3, 3, 3)), int64(ones(3, 3, 3))), true);
%=============================================================================
assert_isequal(isequal(int8(ones(3, 3, 3)), int16(ones(3, 3, 3))), true);
assert_isequal(isequal(int16(ones(3, 3, 3)), int32(ones(3, 3, 3))), true);
assert_isequal(isequal(int32(ones(3, 3, 3)), int64(ones(3, 3, 3))), true);
assert_isequal(isequal(int64(ones(3, 3, 3)), int8(ones(3, 3, 3))), true);
%=============================================================================
A = string(NaN);
B = string(NaN);
assert_isequal(isequal(A, B), true);
%=============================================================================
A = string(NaN);
B = "";
assert_isequal(isequal(A, B), false);
%=============================================================================
