%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% numeric arrays are searched value by value, characters by their codes
asserts.isequal(strfind([1 2 3 2], 2), [2 4]);
asserts.isequal(strfind([1 2 3], [2 3]), 2);
asserts.isequal(strfind('abc', [98 99]), 2);
asserts.isequal(strfind([97 98 99], 'bc'), 2);
asserts.isequal(strfind(int8([1 2 3]), 2), 2);
asserts.isequal(strfind([1 2 3], int8(2)), 2);
asserts.isequal(strfind(true(1, 3), true), [1 2 3]);
asserts.isequal(strfind([1 2 3], []), []);
%=============================================================================
% the option name is case-insensitive, its value may be numeric
asserts.isequal(strfind('abc', 'bc', 'forcecelloutput', false), 2);
asserts.isequal(strfind('abc', 'bc', 'ForceCellOutput', 1), {2});
%=============================================================================
% a scalar cell holds the pattern, an empty pattern finds nothing
asserts.isequal(strfind('a', {'a'}), 1);
asserts.isequal(strfind('abc', char(zeros(1, 0))), []);
%=============================================================================
% strings, cells, complex values and options.
asserts.isequal(strfind(5, "aa"), zeros(0, 0));
asserts.isequal(strfind([97 98 99], "bc"), zeros(0, 0));
asserts.isequal(strfind("abc", 98), 2);
asserts.isequal(strfind("abc", [98 99]), 2);
asserts.isequal(strfind({'abc'}, 98), {2});
asserts.isequal(strfind({'abc', 'xbx'}, [98 120]), {zeros(0, 0), 2});
asserts.isequal(strfind(zeros(1, 0), 1), zeros(0, 0));
asserts.isequal(strfind([1+2i 3], 3), zeros(0, 0));
asserts.isequal(strfind('abcabc', 'bc', 'ForceCellOutput', true), {[2 5]});
asserts.isequal(strfind('abcabc', 'bc', 'forcecelloutput', 1), {[2 5]});
asserts.isequal(strfind('abcabc', 'bc', 'ForceCellOutput', int8(1)), {[2 5]});
asserts.isequal(strfind({'abc', 'bcb'}, 'bc', 'ForceCellOutput', true), {2, 1});
asserts.isequal(strfind('', {''}), zeros(0, 0));
asserts.isequal(strfind('abc', {''}), zeros(0, 0));
asserts.isequal(strfind(single([1 2 3]), 2), 2);
asserts.isequal(strfind([1 2 3 2 3], [2 3]), [2 4]);
%=============================================================================
