%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% character matrices are concatenated row by row, a single row is reused
asserts.isequal(strcat(['a'; 'b'], 'c'), ['ac'; 'bc']);
asserts.isequal(strcat(['a'; 'b'], ['c'; 'd']), ['ac'; 'bd']);
asserts.isequal(strcat(['a'; 'b'], ['c', 'd']), ['acd'; 'bcd']);
asserts.isequal(strcat(['a'; 'b'], 'cd'), ['acd'; 'bcd']);
asserts.isequal(strcat(['a '; 'b '], 'c'), ['ac'; 'bc']);
asserts.isequal(strcat(['a '; 'bb'], 'c'), ['ac '; 'bbc']);
asserts.isequal(strcat('a', [66; 67]), ['aB'; 'aC']);
asserts.isequal(strcat('ab', [66; 67]), ['abB'; 'abC']);
asserts.checkerror('R = strcat([''a''; ''b''], [''c''; ''d''; ''e'']);', 'All character array inputs must have the same number of rows.', 'Nelson:string:strcat:rowCountMismatch');
%=============================================================================
% numbers are taken as character codes
asserts.isequal(strcat('a', 65), 'aA');
asserts.isequal(strcat(65, 66), 'AB');
asserts.isequal(strcat('a', [66 67]), 'aBC');
asserts.isequal(strcat(int8(65), 'b'), 'Ab');
asserts.isequal(strcat('a', 5), ['a', char(5)]);
%=============================================================================
