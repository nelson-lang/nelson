%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% max and min of char data return double values; text is never a second operand.
%=============================================================================
C = ['AB'; 'DC'];
% reductions
assert_isequal(max('AC'), 67);
assert_isequal(min('AC'), 65);
assert_isequal(max('A'), 65);
assert_isequal(min('A'), 65);
assert_isequal(max(C), [68 67]);
assert_isequal(min(C), [65 66]);
assert_isequal(max(C, [], 2), [66; 68]);
assert_isequal(min(C, [], 2), [65; 67]);
assert_isequal(max(C, [], 3), double(C));
assert_isequal(max('AC', [], 1), [65 67]);
assert_isequal(min('A', [], 2), 65);
assert_isequal(max(C, [], 'all'), 68);
assert_isequal(max('A', [], 'all'), 65);
assert_isequal(min(C, [], [1 2]), 65);
assert_isequal(max('AC', [], 'omitnan'), 67);
assert_isequal(min('AC', [], 'includenan'), 65);
% index output
[m, i] = max(C);
assert_isequal(m, [68 67]);
assert_isequal(i, [2 2]);
[m, i] = min('CAB', [], 'all');
assert_isequal(m, 65);
assert_isequal(i, 2);
[m, i] = max(C, [], 'linear');
assert_isequal(m, [68 67]);
assert_isequal(i, [2 4]);
[m, i] = min(C, [], [1 2], 'linear');
assert_isequal(m, 65);
assert_isequal(i, 1);
[m, i] = max('A');
assert_isequal(m, 65);
assert_isequal(i, 1);
% empty inputs
assert_isequal(max(''), []);
assert_isequal(min(char(zeros(0, 3))), zeros(0, 3));
assert_isequal(max(char(zeros(3, 0))), zeros(1, 0));
assert_isequal(max(char(zeros(0, 3)), [], 2), zeros(0, 1));
assert_isequal(max('', [], 'all'), []);
[m, i] = max('');
assert_isequal(m, []);
assert_isequal(i, []);
assert_isequal(max('A', []), []);
% char with a numeric second operand
assert_isequal(max('AC', 66), [66 67]);
assert_isequal(min('AC', 66), [65 66]);
assert_isequal(max('AC', 66.5), [66.5 67]);
assert_isequal(max('AC', single(66)), single([66 67]));
assert_isequal(max('AC', true), [65 67]);
assert_isequal(max('AC', NaN), [65 67]);
assert_isequal(max('AC', NaN, 'includenan'), [NaN NaN]);
assert_isequal(max('AC', single(NaN), 'includenan'), single([NaN NaN]));
assert_isequal(max('AC', 1 + 2i), [65 67]);
assert_isequal(max('A', 66, 'omitnan'), 66);
% char with a sparse second operand gives a sparse double
R = max('AC', sparse(66));
assert_istrue(issparse(R));
assert_isequal(full(R), [66 67]);
R = min('AC', sparse(66));
assert_istrue(issparse(R));
assert_isequal(full(R), [65 66]);
R = max(['AB'; 'CD'], sparse([1 0; 0 100]));
assert_istrue(issparse(R));
assert_isequal(full(R), [65 66; 67 100]);
R = max('AC', sparse(true));
assert_istrue(issparse(R));
assert_isequal(full(R), [65 67]);
R = max(char(zeros(0, 2)), sparse(66));
assert_istrue(issparse(R));
assert_isequal(size(R), [0 2]);
% text as second operand
assert_checkerror('max(''AC'', ''B'')', ...
  'Invalid second argument. To pass extra arguments, call max(A,[],...).', ...
  'Nelson:data_analysis:secondInputText');
assert_checkerror('min(''AC'', ''B'')', ...
  'Invalid second argument. To pass extra arguments, call min(A,[],...).', ...
  'Nelson:data_analysis:secondInputText');
assert_checkerror('max(66, ''AC'')', ...
  'Invalid second argument. To pass extra arguments, call max(A,[],...).', ...
  'Nelson:data_analysis:secondInputText');
assert_checkerror('max(''AC'', "B")', ...
  'Invalid second argument. To pass extra arguments, call max(A,[],...).', ...
  'Nelson:data_analysis:secondInputText');
assert_checkerror('max(5, ''B'', ''omitnan'')', ...
  'Invalid second argument. To pass extra arguments, call max(A,[],...).', ...
  'Nelson:data_analysis:secondInputText');
assert_checkerror('max(5, char(zeros(1, 0)))', ...
  'Invalid second argument. To pass extra arguments, call max(A,[],...).', ...
  'Nelson:data_analysis:secondInputText');
assert_checkerror('max(5, ''omitnan'')', ...
  'Invalid second argument. To give an option, call max(A,[],option).', ...
  'Nelson:data_analysis:secondInputFlag');
assert_checkerror('max(5, ''al'')', ...
  'Invalid second argument. To reduce over all elements, call max(A,[],''all'').', ...
  'Nelson:data_analysis:secondInputAll');
assert_checkerror('max('''', ''A'')', ...
  'Invalid second argument. To reduce over all elements, call max(A,[],''all'').', ...
  'Nelson:data_analysis:secondInputAll');
assert_checkerror('max(5, [''a''; ''b''])', 'Invalid data type for the second input array.', ...
  'Nelson:data_analysis:secondInputType');
assert_checkerror('max(''AB'', [''A''; ''Z''])', 'Invalid data type for the second input array.', ...
  'Nelson:data_analysis:secondInputType');
% char data cannot mix with integers
msg = 'Integers can only be combined with integers of the same class, or scalar doubles.';
assert_checkerror('max(''AC'', int8(66))', msg, 'Nelson:interpreter:integerClassMismatch');
assert_checkerror('min(''AC'', uint16(66))', msg, 'Nelson:interpreter:integerClassMismatch');
% accumarray with a function handle wrapping min sees one class per group
assert_isequal(accumarray([1; 2; 1], 'ABC', [], @(x) min(x)), [65; 66]);
%=============================================================================
