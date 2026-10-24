%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% several patterns are applied one after the other to the same text
asserts.isequal(regexprep('ab', {'a', 'b'}, {'1', '2'}), '12');
asserts.isequal(regexprep('ab', {'a', 'b'}, 'X'), 'XX');
asserts.isequal(regexprep('ab', {'a', 'b'}, {'1'}), '11');
asserts.isequal(regexprep('a,b', {',', 'b'}, {';', 'c'}), 'a;c');
asserts.isequal(regexprep('ab', {'a', 'b'}, {'b', 'a'}), 'aa');
asserts.isequal(regexprep('ab', {'(a)', 'b'}, {'$1$1', 'X'}), 'aaX');
asserts.isequal(regexprep('ab', 'a', {'X', 'Y'}), 'Xb');
asserts.isequal(regexprep('ab', {'a'}, {'1', '2'}), '1b');
asserts.isequal(regexprep({'ab', 'cd'}, 'a', {'X', 'Y'}), {'Xb', 'cd'});
asserts.isequal(regexprep({'ab', 'cd'}, {'a', 'd'}, {'1', '2'}), {'1b', 'c2'});
%=============================================================================
% escape sequences of the replacement, unknown references stay literal
asserts.isequal(regexprep('abc', 'b', '\x41'), 'aAc');
asserts.isequal(regexprep('abc', 'b', '\101'), 'aAc');
asserts.isequal(regexprep('abc', 'b', '\a'), ['a', char(7), 'c']);
asserts.isequal(regexprep('abc', 'b', '\0'), ['a', char(0), 'c']);
asserts.isequal(regexprep('abc', 'b', '$2'), 'a$2c');
asserts.isequal(regexprep('abc', '(b)', '$2'), 'a$2c');
asserts.isequal(regexprep('abc', '(?<n>b)', '$<m>'), 'a$<m>c');
asserts.isequal(regexprep('ab', 'a(x)?', '[$1]'), '[]b');
asserts.isequal(regexprep('ab', '(a)(x)?', '[$2]'), '[]b');
%=============================================================================
% an empty match right after a non-empty one is not replaced
asserts.isequal(regexprep('aaa', 'a*', 'X', 'emptymatch'), 'X');
asserts.isequal(regexprep('baaa', 'a*', 'X', 'emptymatch'), 'XbX');
%=============================================================================
% a group that took no part in the match gives a literal reference (alternatives);
% $N reads digits while N is a group of the pattern.
asserts.isequal(regexprep('ab', '(a)|b', '$1'), 'a$1');
asserts.isequal(regexprep('ab', 'a|(b)', '<$1>'), '<$1><b>');
asserts.isequal(regexprep('ab', '(a)|(b)', '$2'), '$2$2');
asserts.isequal(regexprep('ab', '(a)|(b)', '$1'), 'ab');
asserts.isequal(regexprep('xay', 'x(a)|y', '$1'), 'a$1');
asserts.isequal(regexprep('c', '(a)|(b)|c', '[$1][$2]'), '[$1][$2]');
asserts.isequal(regexprep('ac', '(a)c|(b)', '[$1][$2]'), '[a][$2]');
asserts.isequal(regexprep('ab', '(a)(x)?', '[$2]'), '[]b');
asserts.isequal(regexprep('ab', '(a)(x)?', '[$3]'), '[$3]b');
asserts.isequal(regexprep('ab', '(a)(b)', '$10'), 'a0');
asserts.isequal(regexprep('ab', '(a)(b)', '$20'), 'b0');
asserts.isequal(regexprep('ab', '(a)(b)', '$11'), 'a1');
asserts.isequal(regexprep('ab', '(a)(b)', '$01'), 'ab1');
asserts.isequal(regexprep('ab', '(a)(b)', '$3'), '$3');
asserts.isequal(regexprep('ab', '(a)', '$12'), 'a2b');
asserts.isequal(regexprep('ab', 'a', '$10'), '$10b');
asserts.isequal(regexprep('abcdefghijk', '(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)', '$10'), 'jk');
asserts.isequal(regexprep('abcdefghijk', '(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)', '$11'), 'a1k');
asserts.isequal(regexprep('abcdefghijk', '(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)', '$100'), 'j0k');
asserts.isequal(regexprep('abcdefghijk', '(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)', '$11'), 'k');
asserts.isequal(regexprep('ab', '(?<n>a)|(?<m>b)', '$<n>-$<m>'), 'a-$<m>$<n>-b');
%=============================================================================
