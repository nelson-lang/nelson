%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(strjoin({'a', 'b'}, ','), 'a,b');
asserts.isequal(strjoin(["a", "b"], ","), "a,b");
asserts.isequal(strjoin({'one', 'two', 'three'}, {' + ', ' = '}), 'one + two = three');
asserts.isequal(strjoin(["北京", "上海"], "到"), "北京到上海");
%=============================================================================
% a character vector delimiter honours escape sequences: an unknown escape
% drops its backslash, a trailing backslash is kept; string and cell
% delimiters are taken literally
asserts.isequal(strjoin({'a', 'b'}, '\n'), ['a', char(10), 'b']);
asserts.isequal(strjoin({'a', 'b'}, '\t'), ['a', char(9), 'b']);
asserts.isequal(strjoin({'a', 'b'}, '\0'), ['a', char(0), 'b']);
asserts.isequal(strjoin({'a', 'b'}, '\\'), 'a\b');
asserts.isequal(strjoin({'a', 'b'}, '\'), 'a\b');
asserts.isequal(strjoin({'a', 'b'}, '\q'), 'aqb');
asserts.isequal(strjoin({'a', 'b'}, '\x41'), 'ax41b');
asserts.isequal(strjoin({'a', 'b'}, '\101'), 'a101b');
asserts.isequal(strjoin({'a', 'b'}, '%d'), 'a%db');
asserts.isequal(strjoin(["a", "b"], "\n"), "a\nb");
asserts.isequal(strjoin({'a', 'b'}, "\n"), 'a\nb');
asserts.isequal(strjoin({'a', 'b', 'c'}, {'\t', '\\'}), 'a\tb\\c');
asserts.isequal(strjoin({'a', 'b', 'c'}, {'\n'}), 'a\nb\nc');
%=============================================================================
% a missing string makes the result missing, the first input must be a
% string array or a cell, the delimiter count must fit
asserts.istrue(ismissing(strjoin(["a", missing], ',')));
asserts.isequal(strjoin({'a', 'b'}, string(missing)), 'ab');
asserts.isequal(strjoin({}, ','), '');
asserts.isequal(strjoin({'a', 'b'}, ''), 'ab');
asserts.isequal(strjoin(string.empty(0, 0), ','), "");
msg = 'First input must be a string array or cell array of character vectors.';
asserts.checkerror('R = strjoin(''abc'', '','');', msg, 'Nelson:string:strjoin:invalidInput');
asserts.checkerror('R = strjoin(5, '','');', msg, 'Nelson:string:strjoin:invalidInput');
msg = 'When the delimiter is a string array or cell array of character vectors, it must be scalar or have one fewer element than the first input.';
asserts.checkerror('R = strjoin({''a'', ''b''}, {''\n'', ''x''});', msg, 'Nelson:string:strjoin:invalidDelimiterCount');
asserts.checkerror('R = strjoin({''a'', ''b''}, {});', msg, 'Nelson:string:strjoin:invalidDelimiterCount');
%=============================================================================
