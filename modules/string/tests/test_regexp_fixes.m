%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% named tokens come in pattern order
names = regexp('a1b2', '(?<l>[a-z])(?<d>\d)', 'names');
asserts.isequal(fieldnames(names), {'l'; 'd'});
asserts.isequal(size(names), [1 2]);
asserts.isequal(names(2).d, '2');
%=============================================================================
% outputs beyond the selected ones follow the default order
[s, e, te] = regexp('ab12', '(\d)(\d)');
asserts.isequal(s, 3);
asserts.isequal(e, 4);
asserts.isequal(te, {[3 3; 4 4]});
[m, s] = regexp('abc', 'b', 'match');
asserts.isequal(m, {'b'});
asserts.isequal(s, 2);
[m, s, e] = regexp('abc', 'b', 'match', 'once');
asserts.isequal(m, 'b');
asserts.isequal(s, 2);
asserts.isequal(e, 2);
[s, e, te] = regexp('abc', 'b', 'once');
asserts.isequal(s, 2);
asserts.isequal(e, 2);
asserts.isequal(size(te), [0 0]);
[s, e, te, m, tok, names, sp] = regexp('ab12', '(\d)(\d)');
asserts.isequal(m, {'12'});
asserts.isequal(tok, {{'1', '2'}});
asserts.isequal(sp, {'ab', ''});
%=============================================================================
% a string subject gives string results
asserts.isequal(regexp("abc", "b", 'match'), "b");
asserts.isequal(regexp("abc", "b", 'match', 'once'), "b");
asserts.istrue(ismissing(regexp("abc", "x", 'match', 'once')));
asserts.isequal(regexp("abc", "(b)", 'tokens'), {"b"});
asserts.isequal(regexp("abc", "(b)", 'tokens', 'once'), "b");
asserts.isequal(regexp("abc", "b", 'split'), ["a", "c"]);
names = regexp("abc", "(?<n>b)", 'names');
asserts.isequal(names.n, "b");
asserts.isequal(regexp(["ab", "cb"], "b", 'match', 'once'), ["b", "b"]);
R = regexp(["ab", "cb"], "x", 'match', 'once');
asserts.istrue(all(ismissing(R)));
asserts.isequal(size(R), [1 2]);
%=============================================================================
% nothing matched: 0x0 results
asserts.isequal(size(regexp('abc', 'x', 'match')), [0 0]);
asserts.isequal(size(regexp('abc', 'x', 'tokens')), [0 0]);
asserts.isequal(size(regexp('abc', 'x', 'names')), [0 0]);
asserts.isequal(size(regexp('abc', 'x', 'match', 'once')), [0 0]);
asserts.isequal(size(regexp('abc', 'x', 'tokens', 'once')), [0 0]);
asserts.isequal(size(regexp('abc', 'x', 'tokenExtents', 'once')), [0 0]);
asserts.isequal(size(regexp('abc', 'b', 'names')), [1 1]);
asserts.isequal(size(regexp('abc', 'b', 'tokens', 'once')), [1 0]);
asserts.isequal(size(regexp('a', 'a', 'once', 'tokens')), [1 0]);
te = regexp('abc', 'b', 'tokenExtents');
asserts.isequal(size(te{1}), [0 0]);
%=============================================================================
% every group of the pattern is a token, set or not
asserts.isequal(regexp('ab', '(a)(x)?', 'tokens'), {{'a', ''}});
asserts.isequal(regexp('ab', '(a)(x)?', 'tokenExtents'), {[1 1; 2 1]});
asserts.isequal(regexp('x', '(a)?x', 'tokens'), {{''}});
asserts.isequal(regexp('x', '(a)?x', 'tokenExtents'), {[1 0]});
asserts.isequal(regexp('a', 'a(b)?(c)?', 'tokens'), {{'', ''}});
%=============================================================================
% an empty match right after a non-empty one is not reported
asserts.isequal(regexp('aaa', 'a*', 'match', 'emptymatch'), {'aaa'});
asserts.isequal(regexp('abc', 'x*', 'match', 'emptymatch'), {'', '', '', ''});
%=============================================================================
% \w follows Unicode letters
asserts.isequal(regexp(['a', char(233), 'b'], '\w+', 'match'), {['a', char(233), 'b']});
asserts.isequal(regexp(char([26085 26412 35486]), '\w', 'match'), {char(26085), char(26412), char(35486)});
%=============================================================================
% tokens of the alternatives that did not match are not reported.
asserts.isequal(regexp('abc', '(a)|(b)', 'tokens'), {{'a'}, {'b'}});
asserts.isequal(regexp('abc', '(a)|(b)', 'tokenExtents'), {[1 1], [2 2]});
asserts.isequal(regexp('abc', '(a)|(b)|(c)', 'tokens'), {{'a'}, {'b'}, {'c'}});
asserts.isequal(regexp('abc', '(?<x>a)|(?<y>b)', 'tokens'), {{'a'}, {'b'}});
asserts.isequal(regexp('b', '(a)|(b)', 'tokens', 'once'), {'b'});
asserts.isequal(regexp('a', '(a)|(b)', 'tokens', 'once'), {'a'});
asserts.isequal(regexp('abab', '(a)|(b)', 'tokenExtents', 'once'), [1 1]);
asserts.isequal(regexp('a', '(a)(b)?|(c)', 'tokens'), {{'a', ''}});
asserts.isequal(regexp('c', '(a)(b)?|(c)', 'tokens'), {{'c'}});
asserts.isequal(regexp('ab', '(a)(b)?|(c)', 'tokens'), {{'a', 'b'}});
asserts.isequal(regexp('a', '(a|b)(c)?', 'tokens'), {{'a', ''}});
asserts.isequal(regexprep('b', '(a)|(b)', '[$1][$2]'), '[b][$2]');
asserts.isequal(regexprep('a', '(a)|(b)', '[$1][$2]'), '[a][$2]');
asserts.isequal(regexprep('ab', '(a)(x)?', '[$2]'), '[]b');
%=============================================================================
% no group took part in the match: no token.
asserts.isequal(regexp('ab', 'a|(b)', 'tokens'), {cell(1, 0), {'b'}});
asserts.isequal(regexp('ab', '(a)|b', 'tokens'), {{'a'}, cell(1, 0)});
asserts.isequal(regexp('ab', '(a)|b', 'tokenExtents'), {[1 1], zeros(0, 0)});
asserts.isequal(regexp('ab', 'a|(b)', 'tokenExtents'), {zeros(0, 0), [2 2]});
asserts.isequal(regexp('a', 'a|(b)', 'tokens', 'once'), cell(1, 0));
asserts.isequal(regexp('b', 'a|(b)', 'tokens', 'once'), {'b'});
asserts.isequal(regexp('b', '(a)|b', 'tokens'), {cell(1, 0)});
asserts.isequal(regexp('c', '(a)|(b)|c', 'tokens'), {cell(1, 0)});
%=============================================================================
