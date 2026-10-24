%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% a character matrix loses its leading and trailing all-blank columns
asserts.isequal(strtrim(['  a'; 'b  ']), ['  a'; 'b  ']);
asserts.isequal(strtrim(['  a  '; '  b  ']), ['a'; 'b']);
asserts.isequal(strtrim(['  a  '; '  bb ']), ['a '; 'bb']);
asserts.isequal(strtrim(['  a  '; ' bb  ']), [' a'; 'bb']);
asserts.isequal(strtrim(char([32 97 32; 32 32 98])), ['a '; ' b']);
asserts.isequal(strtrim(char([9 97 10; 32 98 13])), ['a'; 'b']);
asserts.isequal(size(strtrim(char(zeros(2, 3) + 32))), [0 0]);
asserts.isequal(size(strtrim(char(zeros(0, 3)))), [0 0]);
asserts.isequal(size(strtrim('   ')), [0 0]);
asserts.isequal(strtrim({'  a  ', ['  b'; 'c  ']}), {'a', ['  b'; 'c  ']});
%=============================================================================
% Unicode space separators are blanks, the no-break space and NUL are not
asserts.isequal(strtrim(char([8192 97 8192])), 'a');
asserts.isequal(strtrim(char([12288 97 12288])), 'a');
asserts.isequal(strtrim(char([5760 97 5760])), 'a');
asserts.isequal(strtrim(char([8232 97 8233])), 'a');
asserts.isequal(strtrim(char([28 97 31])), 'a');
asserts.isequal(strtrim(char([160 97 160])), char([160 97 160]));
asserts.isequal(strtrim(char([0 97 0])), char([0 97 0]));
%=============================================================================
% cells keep nested cells and trim string elements
asserts.isequal(strtrim({'  a  ', {'b'}}), {'a', {'b'}});
asserts.isequal(strtrim({'  a  ', "b "}), {'a', "b"});
R = strtrim(["  a", "b  ", missing]);
asserts.isequal(R(1:2), ["a", "b"]);
asserts.istrue(ismissing(R(3)));
asserts.checkerror('R = strtrim(reshape(''  ab'', 1, 1, 4));', 'Only character arrays with up to two dimensions are accepted.', 'Nelson:string:trim:notTwoDimensional');
asserts.checkerror('R = strtrim({''  a'', 5});', 'Wrong type for argument #1: cell of strings expected.', 'Nelson:invalidType');
%=============================================================================
