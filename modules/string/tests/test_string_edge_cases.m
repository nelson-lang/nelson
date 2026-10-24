%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
asserts.isequal(split(["a,b"; "c,d"], ","), ["a", "b"; "c", "d"]);
asserts.isequal(split(' a  b '), {'', 'a', 'b', ''}');
asserts.isequal(split("", ","), "");
asserts.checkerror('split(missing, ",")', sprintf(_('Wrong type for argument #%d: %s expected.'), 1, 'string or cell'));
asserts.isequal(strsplit('a,b;c', {',', ';'}), {'a', 'b', 'c'});
asserts.isequal(strsplit(',a,,', ',', 'CollapseDelimiters', false), {'', 'a', '', ''});
asserts.isequal(strsplit(',a,,', ',', 'CollapseDelimiters', true), {'', 'a', ''});
[parts, matches] = strsplit('a1b22c', '\d+', 'DelimiterType', 'RegularExpression');
asserts.isequal(parts, {'a', 'b', 'c'});
asserts.isequal(matches, {'1', '22'});
asserts.isequal(strjoin({}, ','), '');
asserts.isequal(strjoin({'a'; 'b'}, '|'), 'a|b');
asserts.isequal(findstr('aaaa', 'aa'), [1 2 3]);
asserts.isequal(findstr('bc', 'abcabc'), [2 5]);
asserts.isequal(findstr('abc', ''), []);
asserts.isequal(strmatch('ab', char('abc', 'abz', 'xx')), [1; 2]);
asserts.isequal(strmatch('', {'a'; ''; 'b'}), [1; 2; 3]);
asserts.isequal(strvcat('', []), '');
asserts.isequal(strvcat({'ab', 'c'}), ['ab'; 'c ']);
[tok, rest] = strtok('  one two');
asserts.isequal(tok, 'one');
asserts.isequal(rest, ' two');
[tok, rest] = strtok('a,b;c', ',;');
asserts.isequal(tok, 'a');
asserts.isequal(rest, ',b;c');
asserts.isequal(erase(["aba", "bbb"; "ccc", "abc"], "b"), ["aa", ""; "ccc", "ac"]);
asserts.isequal(erase({'abc', 'bbb'}, 'b'), {'ac', ''});
asserts.isequal(extract(["abc", "de"], 2), ["b", "e"]);
asserts.isequal(extract('a12b34', digitsPattern(2)), {'12'; '34'});
asserts.isequal(extractAfter(["a:b"; "c:d"], ":"), ["b"; "d"]);
asserts.isequal(extractBefore(["a:b"; "c:d"], ":"), ["a"; "c"]);
asserts.isequal(splitlines("a" + newline() + "b" + char(13) + "c"), ["a"; "b"; "c"]);
%=============================================================================
