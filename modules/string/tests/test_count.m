%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
asserts.isequal(nargin('count'), -3);
asserts.isequal(nargout('count'), 1);
%=============================================================================
res = count('nelson', 'n');
asserts.isequal(res, 2);
%=============================================================================
c1 = 'time';
c2 = {'Once', 'upon'; 'a', 'time'};
res = count(c1, c2);
ref = 1;
asserts.isequal(res, ref);
%=============================================================================
res = count(c2, c1);
ref = [0, 0; 0, 1];
asserts.isequal(res, ref);
%=============================================================================
c1 = {'abs.docx', 'data.gz', 'code.m', 'results.nlf'};
c2 = {'abs', 'data'};
res = count(c1, c2);
ref = [1, 1, 0, 0];
asserts.isequal(res, ref);
%=============================================================================
c1 = {'abs.docx', 'data.gz', 'ABS.m', 'DAta.nlf'};
c2 = {'abs', 'data'};
res = count(c1, c2, 'IgnoreCase', true);
ref = [1, 1, 1, 1];
asserts.isequal(res, ref);
%=============================================================================
c1 = {'blue green red green blue red green';
  'yellow red red green green red'};
res = count(c1, 'green');
ref = [3; 2];
asserts.isequal(res, ref);
%=============================================================================
c1 = {'abs.docx', 'data.gz', 'code.m', 'results.nlf'};
c2 = {1};
asserts.checkerror('res = count(c1, c2);', _('character vector or cell of strings expected.'));
%=============================================================================
c1 = 'time';
c2 = ["Once", "upon"; "a", "time"];
res = count(c1, c2);
ref = 1;
asserts.isequal(res, ref);
%=============================================================================
c1 = ["abs.docx", "data.gz"; "ABS.m", "DAta.nlf"];
c2 = ["abs", "data"];
res = count(c1, c2, 'IgnoreCase', true);
ref = [1, 1; 1, 1];
asserts.isequal(res, ref);
%=============================================================================
s = ["ababa", missing, "cc"];
res = count(s, "a");
ref = [3, 0, 0];
asserts.isequal(res, ref);
%=============================================================================
res = count(s, "c");
ref = [0, 0, 2];
asserts.isequal(res, ref);
%=============================================================================
res = count(s, ["a", "c"]);
ref = [3, 0, 2];
asserts.isequal(res, ref);
%=============================================================================
res = count(string(missing), "a");
asserts.isequal(res, 0);
%=============================================================================
% IgnoreCase options, several patterns are searched at once without overlap.
asserts.isequal(count('abcabc', 'B', 'ignorecase', true), 2);
asserts.isequal(count('abcabc', 'B', 'IgnoreCase', 1), 2);
asserts.isequal(count('abcabc', 'B', 'IgnoreCase', int8(1)), 2);
asserts.isequal(count('abcabc', ''), 7);
asserts.isequal(count('', ''), 1);
asserts.isequal(count('aaaa', 'aa'), 2);
asserts.isequal(count('abcabc', {'a', 'b'}), 4);
asserts.isequal(count('abcabc', {'ab', 'b'}), 2);
asserts.isequal(count('abcabc', {'b', 'ab'}), 2);
asserts.isequal(count('abab', {'ab', 'ba'}), 2);
asserts.isequal(count('abab', {'ba', 'ab'}), 2);
asserts.isequal(count('abab', {'aba', 'bab'}), 1);
asserts.isequal(count('abc', {'a', ''}), 3);
asserts.isequal(count('abc', {'', 'a'}), 4);
asserts.isequal(count({'abc', 'bcb'}, {'b', 'c'}), [2 3]);
asserts.isequal(count(['a' char(233)], char(201), 'IgnoreCase', true), 1);
asserts.isequal(count(["abc" missing], 'b'), [1 0]);
%=============================================================================
