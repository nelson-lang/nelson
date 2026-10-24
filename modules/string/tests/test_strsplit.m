%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(strsplit('a,b,c', ','), {'a', 'b', 'c'});
asserts.isequal(strsplit('a,,b', ',', 'CollapseDelimiters', true), {'a', 'b'});
[parts, matches] = strsplit('a1b22c', '\d+', 'DelimiterType', 'RegularExpression');
asserts.isequal(parts, {'a', 'b', 'c'});
asserts.isequal(matches, {'1', '22'});
asserts.isequal(strsplit('a,,b', ',', 'CollapseDelimiters', false), {'a', '', 'b'});
asserts.isequal(strsplit('北京;上海;深圳', ';'), {'北京', '上海', '深圳'});
%=============================================================================
% the delimiter honours escape sequences (an unknown escape drops its
% backslash)
asserts.isequal(strsplit(['a', char(10), 'b'], '\n'), {'a', 'b'});
asserts.isequal(strsplit(['a', char(9), 'b', char(9), 'c'], '\t'), {'a', 'b', 'c'});
asserts.isequal(strsplit('a\nb', '\n'), {'a\nb'});
asserts.isequal(strsplit('a\b', '\\'), {'a', 'b'});
asserts.isequal(strsplit('a\b', '\'), {'a', 'b'});
asserts.isequal(strsplit('a\qb', '\q'), {'a\', 'b'});
asserts.isequal(strsplit('aqb', '\q'), {'a', 'b'});
asserts.isequal(strsplit(['a', char(0), 'b'], '\0'), {'a', 'b'});
%=============================================================================
% a string input gives string outputs, a missing string stays missing
asserts.isequal(strsplit("a,b", ","), ["a", "b"]);
asserts.isequal(strsplit("a,b", ','), ["a", "b"]);
asserts.isequal(strsplit("a b"), ["a", "b"]);
asserts.isequal(strsplit(""), "");
asserts.istrue(ismissing(strsplit(string(missing), ",")));
[parts, matches] = strsplit("a,b", ",");
asserts.isequal(matches, ",");
[parts, matches] = strsplit("a", ",");
asserts.isequal(size(matches), [0 0]);
asserts.istrue(isstring(matches));
[parts, matches] = strsplit('abc');
asserts.isequal(size(matches), [0 0]);
asserts.istrue(iscell(matches));
%=============================================================================
% option names are case-insensitive and may be abbreviated, the collapse
% value may be numeric
asserts.isequal(strsplit('a,b', ',', 'collapsedelimiters', false), {'a', 'b'});
asserts.isequal(strsplit('a,b', ',', 'Collapse', true), {'a', 'b'});
asserts.isequal(strsplit('a,b,c', ',', 'CollapseDelimiters', 0), {'a', 'b', 'c'});
asserts.isequal(strsplit('a,,b', ',', 'CollapseDelimiters', int8(0)), {'a', '', 'b'});
asserts.isequal(strsplit('a,b', ',', 'delimitertype', 'simple'), {'a', 'b'});
asserts.checkerror('R = strsplit(''a,b'', '','', ''CollapseDelimiters'');', 'Name-value pair arguments require a name followed by a value.', 'Nelson:string:strsplit:nameValue');
asserts.checkerror('R = strsplit(''a,b'', '','', ''DelimiterType'', ''RegExp'');', 'DelimiterType must be ''Simple'' or ''RegularExpression''.', 'Nelson:string:strsplit:invalidOption');
%=============================================================================
% an empty delimiter never splits, the inputs are checked
asserts.isequal(strsplit('a,b', ''), {'a,b'});
asserts.isequal(strsplit('a b', ''), {'a b'});
asserts.isequal(strsplit('a,b', {}), {'a,b'});
asserts.isequal(strsplit('a,b', {'', ','}), {'a', 'b'});
asserts.isequal(strsplit(char([97 8192 98])), {'a', 'b'});
asserts.isequal(strsplit(char([97 160 98])), {'a', 'b'});
asserts.checkerror('R = strsplit(["a,b", "c,d"], '','');', 'First input must be either a character vector or a string scalar.', 'Nelson:string:strsplit:invalidInput');
asserts.checkerror('R = strsplit(5, '','');', 'First input must be either a character vector or a string scalar.', 'Nelson:string:strsplit:invalidInput');
msg = 'The delimiter must be either a character vector, a cell array of character vectors, or a string array.';
asserts.checkerror('R = strsplit(''a,b'', 5);', msg, 'Nelson:string:strsplit:invalidDelimiter');
asserts.checkerror('R = strsplit(''a,b'', []);', msg, 'Nelson:string:strsplit:invalidDelimiter');
%=============================================================================
