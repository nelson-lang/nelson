%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 16-->
asserts.isequal(nargin('regexp'), -2);
asserts.isequal(nargout('regexp'), -1);
%=============================================================================
idx = regexp('bat cat can car coat court CUT ct CAT-scan', 'c[aeiou]+t');
asserts.isequal(idx, [5 17]);
%=============================================================================
[s, e] = regexp('bat cat coat', 'c[aeiou]+t');
asserts.isequal(s, [5 9]);
asserts.isequal(e, [7 12]);
%=============================================================================
m = regexp('EXTRA! The regexp function helps you relax.', '\w*x\w*', 'match');
asserts.isequal(m, {'regexp', 'relax'});
%=============================================================================
sp = regexp('Split ^this text into ^several pieces', '\^', 'split');
asserts.isequal(sp, {'Split ', 'this text into ', 'several pieces'});
%=============================================================================
[match, noMatch] = regexp('She sells sea shells by the seashore.', '[Ss]h.', 'match', 'split');
asserts.isequal(match, {'She', 'she', 'sho'});
asserts.isequal(noMatch, {char([]), ' sells sea ', 'lls by the sea', 're.'});
%=============================================================================
[tok, mat] = regexp('<title>My Title</title><p>Here is some text.</p>', '<(\w+).*>.*</\1>', 'tokens', 'match');
asserts.isequal(tok, {{'title'}, {'p'}});
asserts.isequal(mat, {'<title>My Title</title>', '<p>Here is some text.</p>'});
%=============================================================================
te = regexp('abc 123 def 45', '(\d+)', 'tokenExtents');
asserts.isequal(te, {[5 7], [13 14]});
%=============================================================================
names = regexp('01/11/2000 20-02-2020', '(?<month>\d+)/(?<day>\d+)/(?<year>\d+)|(?<day>\d+)-(?<month>\d+)-(?<year>\d+)', 'names');
asserts.isequal(names(1).month, '01');
asserts.isequal(names(1).day, '11');
asserts.isequal(names(2).month, '02');
asserts.isequal(names(2).day, '20');
%=============================================================================
txt = {'Madrid, Spain'; 'Romeo and Juliet'; 'NELSON is great'};
cap = regexp(txt, '[A-Z]');
asserts.isequal(cap, {[1 9]; [1 11]; [1 2 3 4 5 6]});
%=============================================================================
c = regexp('bat cat coat', 'c[aeiou]+t', 'forceCellOutput');
asserts.isequal(c, {[5 9]});
%=============================================================================
once = regexp('aba aba', 'a\w+a', 'match', 'once');
asserts.isequal(once, 'aba');
%=============================================================================
line = sprintf('abc\n de');
m1 = regexp(line, '.*', 'match', 'dotexceptnewline');
asserts.isequal(m1, {'abc', ' de'});
%=============================================================================
asserts.isequal(regexp('abc', 'abc'), 1);
asserts.checkerror('regexp(''abc'', ''abc'', ''r'');', _('Unknown regular expression option.'));
%=============================================================================
t = 'aaa aab aac aad aae';
[start_pos, end_pos, match_str] = regexp(t, 'aa', 'start', 'end', 'match', 'once');
asserts.isequal(start_pos, 1);
asserts.isequal(end_pos, 2);
asserts.isequal(match_str, 'aa');
%=============================================================================
cases = {
    'abc', 'abc', 1;
    'xbc', 'abc', [];
    'axc', 'abc', [];
    'abx', 'abc', [];
    'xabcy', 'abc', 2;
    'ababc', 'abc', 3;
    'abc', 'ab*c', 1;
    'abc', 'ab*bc', 1;
    'abbc', 'ab*bc', 1;
    'abbbbc', 'ab*bc', 1;
    'abbbbc', '.{1}', 1:6;
    'abbbbc', '.{3,4}', 1;
    'abbbbc', 'ab{0,}bc', 1;
    'abbc', 'ab+bc', 1;
    'abc', 'ab+bc', [];
    'abq', 'ab+bc', [];
    'abq', 'ab{1,}bc', [];
    'abbbbc', 'ab+bc', 1;
    'abbbbc', 'ab{1,}bc', 1;
    'abbbbc', 'ab{1,3}bc', 1;
    'abbbbc', 'ab{3,4}bc', 1;
    'abbbbc', 'ab{4,5}bc', [];
    'abbc', 'ab?bc', 1;
    'abc', 'ab?bc', 1;
    'abc', 'ab{0,1}bc', 1;
    'abbbbc', 'ab?bc', [];
    'abc', 'ab?c', 1;
    'abc', 'ab{0,1}c', 1;
    'abc', '^abc$', 1;
    'abcc', '^abc$', [];
    'abcc', '^abc', 1;
    'aabc', '^abc$', [];
    'aabc', 'abc$', 2;
    'aabcd', 'abc$', [];
    'abc', 'a.c', 1;
    'axc', 'a.c', 1;
    'axyzc', 'a.*c', 1;
    'axyzd', 'a.*c', [];
    'abc', 'a[bc]d', [];
    'abd', 'a[bc]d', 1;
    'abd', 'a[b-d]e', [];
    'ace', 'a[b-d]e', 1;
    'aac', 'a[b-d]', 2;
    'a-', 'a[-b]', 1;
    'a-', 'a[b-]', 1;
    'a]', 'a]', 1;
    'a]b', 'a[]]b', 1;
    'aed', 'a[^bc]d', 1;
    'abd', 'a[^bc]d', [];
    'adc', 'a[^-b]c', 1;
    'a-c', 'a[^-b]c', [];
    'a]c', 'a[^]b]c', [];
    'adc', 'a[^]b]c', 1;
    'a-', '\ba\b', 1;
    '-a', '\ba\b', 2;
    '-a-', '\ba\b', 2;
    'xy', '\by\b', [];
    'yz', '\by\b', [];
    'xyz', '\by\b', [];
    'a-', '\Ba\B', [];
    '-a', '\Ba\B', [];
    '-a-', '\Ba\B', [];
    'xy', '\By\b', 2;
    'yz', '\by\B', 1;
    'xyz', '\By\B', 2;
    'a', '\w', 1;
    '-', '\w', [];
    'a', '\W', [];
    '-', '\W', 1;
    'a b', 'a\sb', 1;
    'a-b', 'a\sb', [];
    'a b', 'a\Sb', [];
    'a-b', 'a\Sb', 1;
    '1', '\d', 1;
    '-', '\d', [];
    '1', '\D', [];
    '-', '\D', 1;
    'a', '[\w]', 1;
    '-', '[\w]', [];
    'a', '[\W]', [];
    '-', '[\W]', 1;
    'a b', 'a[\s]b', 1;
    'a-b', 'a[\s]b', [];
    'a b', 'a[\S]b', [];
    'a-b', 'a[\S]b', 1;
    '1', '[\d]', 1;
    '-', '[\d]', [];
    '1', '[\D]', [];
    '-', '[\D]', 1;
    'abc', 'ab|cd', 1;
    'abcd', 'ab|cd', [1 3];
    'b', '$b', [];
    'ab', 'a\(*b', 1;
    'a((b', 'a\(*b', 1;
    'aabbabc', 'a+b+c', 5;
    'aabbabc', 'a{1,}b{1,}c', 5;
    'abcabc', 'a.+?c', [1 4];
    'cde', '[^ab]*', 1;
    '', 'abc', [];
    '', 'a*', [];
    'e', 'a|b|c|d|e', 1;
    'abcdefg', 'abcd*efg', 1;
    'xabyabbbz', 'ab*', [2 5];
    'xayabbbz', 'ab*', [2 4];
    'hij', '[abhgefdc]ij', 1;
    'abcde', '^(ab|cd)e', [];
    'adcdcde', 'a[bcd]*dcdcde', 1;
    'adcdcde', 'a[bcd]+dcdcde', [];
    'alpha', '[a-zA-Z_][a-zA-Z0-9_]*', 1;
    'effg', '(bc+d$|ef*g.|h?i(j|k))', [];
    'bcdd', '(bc+d$|ef*g.|h?i(j|k))', [];
    'aa', '((((((((((a))))))))))\10', 1;
    'aa', '((((((((((a))))))))))\041', [];
    'a!', '((((((((((a))))))))))\041', 1;
    'a', '(((((((((a)))))))))', 1;
    'uh-uh', 'multiple words of text', [];
    'multiple words, yeah', 'multiple words', 1;
    'ab', '[k]', [];
    'ac', 'a[-]?c', 1;
    'a', '(a)|\1', 1;
    'x', '(a)|\1', [];
    'aaxabxbaxbbx', '((\3|b)\2(a)x)+', [];
    'abad', 'a(?!b).', 3;
    'abad', 'a(?=d).', 3;
    'abad', 'a(?=c|d).', 3;
    '<&OUT', '^[<>]&', 1;
    'ab', '(?<=a)b', 2;
    'cb', '(?<=a)b', [];
    'b', '(?<=a)b', [];
    'ab', '(?<!c)b', 2;
    'cb', '(?<!c)b', [];
    'b', '(?<!c)b', 1;
    'aba', '(?:..)*a', 1;
    'aba', '(?:..)*?a', [1 3];
    'abc', '^(?:b|a(?=(.)))*\1', 1;
    'ab', '(?:(?i)a)b', 1;
    'Ab', '(?:(?i)a)b', 1;
    'aB', '(?:(?i)a)b', [];
    'ab', '(?i:a)b', 1;
    'Ab', '(?i:a)b', 1;
    'aB', '(?i:a)b', [];
    'ab', '(?:(?-i)a)b', 1;
    'aB', '(?:(?-i)a)b', [];
    'cabbbb', '(?:c|d)(?:)(?:a(?:)(?:b)(?:b(?:))(?:b(?:)(?:b)))', 1;
    'foobar1234baz', 'foo\w*\d{4}baz', 1;
    'x~~', 'x(~~)*(?:(?:F)?)?', 1;
    'aaac', '^a(?#xxx){3}c', 1;
    'dbcb', '(?<![cd])b', [];
    'dbaacb', '(?<![cd])[ab]', [3 4];
    'cdaccb', '(?<!cd)[ab]', 6;
    'a--', '^(?:a?b?)*$', [];
    'x', 'a*abc?xyz+pqr{3}ab{2,}xy{4,5}pq{0,6}AB{0,}zz', [];
    'foo.bart', 'foo.bart', 1;
    'xxxtt', 'tt+$', 4;
    'aaaXbX', '\GX.*X', [];
    'Changes', '\.c(pp|xx|c)?$', [];
    'IO.c', '\.c(pp|xx|c)?$', 3;
    'C:/', '^([a-z]:)', [];
    'ab', '(^|a)b', 1;
    'abcab', '(\w)?(abc)\1b', [];
    'a,b,c', '^(?:.,){2}c', 1;
    'a,b,c', '^(?:[^,]*,){2}c', 1;
    '', '(?i)', [];
    'a', '^(a)?(?(1)a|b)+$', [];
    'x1', '^(0+)?(?:x(1))?', 1;
    '012cxx0190', '^([0-9a-fA-F]+)(?:x([0-9a-fA-F]+)?)(?:x([0-9a-fA-F]+))?', 1;
    'aaaacccc', '((?:aaaa|bbbb)cccc)?', 1;
    'bbbbcccc', '((?:aaaa|bbbb)cccc)?', 1;
    'a', '\ba', 1;
    'AbCd', 'ab(?i)cd', [];
    'abCd', 'ab(?i)cd', 1;
    'Oo', '^(o)(?!.*\1)', [];
    '2', '2(]*)?$\1', 1;
    '......abef', '.*a(?!(b|cd)*e).*f', [];
    'fools', '(foo|fool|x.|money|parted)$', [];
    'fools', '(x.|foo|fool|x.|money|parted|y.)$', [];
    'fools', '(foo|fool|money|parted)$', [];
    'nelson-5.0', '^nelson-[5-9].[0-9](.[0-9])?(-(alpha-|beta-|rc)([0-9])?)?$', 1;
    'nelson-5.0.1', '^nelson-[5-9].[0-9](.[0-9])?(-(alpha-|beta-|rc)([0-9])?)?$', 1;
    'nelson-5.0-alpha-1', '^nelson-[5-9].[0-9](.[0-9])?(-(alpha-|beta-|rc)([0-9])?)?$', 1;
    'nelson-5.0-alpha1', '^nelson-[5-9].[0-9](.[0-9])?(-(alpha-|beta-|rc)([0-9])?)?$', [];
    'nelson-5.0-rc1', '^nelson-[5-9].[0-9](.[0-9])?(-(alpha-|beta-|rc)([0-9])?)?$', 1;
    'nelson-5.0-rc-1', '^nelson-[5-9].[0-9](.[0-9])?(-(alpha-|beta-|rc)([0-9])?)?$', [];
    'nelson-SE-trunk-27490', '^nelson-[5-9].[0-9](.[0-9])?(-(alpha-|beta-|rc)([0-9])?)?$', [];
};
for k = 1:size(cases, 1)
  computed = regexp(cases{k, 1}, cases{k, 2});
  [ok, msg] = asserts.isequal(computed, cases{k, 3});
  if ~ok
    error(['regexp nelson compatibility case ', int2str(k), ': ', msg]);
  end
end
%=============================================================================
icases = {
    'ABC', 'abc', 1;
    'XBC', 'abc', [];
    'AXC', 'abc', [];
    'ABX', 'abc', [];
    'XABCY', 'abc', 2;
    'ABABC', 'abc', 3;
    'ABC', 'ab*c', 1;
    'ABBBBC', 'ab*?bc', 1;
    'ABBC', 'ab+?bc', 1;
    'ABC', 'ab+bc', [];
    'ABBBBC', 'ab{1,3}?bc', 1;
    'ABBBBC', 'ab{4,5}?bc', [];
    'ABCABC', 'a.+?c', [1 4];
    'ABCABC', 'a.*?c', [1 4];
    'XABYABBBZ', 'ab*', [2 5];
    'XAYABBBZ', 'ab*', [2 4];
    'MULTIPLE WORDS, YEAH', 'multiple words', 1;
    'AB', '[k]', [];
    'AC', 'a[-]?c', 1;
    'IO.c', '\.c(pp|xx|c)?$', 3;
};
for k = 1:size(icases, 1)
  computed = regexpi(icases{k, 1}, icases{k, 2});
  [ok, msg] = asserts.isequal(computed, icases{k, 3});
  if ~ok
    error(['regexpi nelson compatibility case ', int2str(k), ': ', msg]);
  end
end
%=============================================================================
asserts.isequal(regexp(['a', char(10), 'b', char(10)], '(?m)^b'), 3);
asserts.isequal(regexp(['a', char(10), 'b', char(10), 'c', char(10)], '^b'), []);
asserts.isequal(regexp(['a', char(10), 'xb', char(10)], '(?!\A)x', 'lineanchors'), 3);
asserts.isequal(regexp(['abcd', char(10), 'dxxx'], '^d[x][x][x]', 'lineanchors'), 6);
asserts.isequal(regexp([char(10), 'x aa'], '^\S\s+aa$', 'lineanchors'), 2);
[s, e, m] = regexp([char(10), 'x aa'], '^\S\s+aa$', 'start', 'end', 'match', 'lineanchors');
asserts.isequal(s, 2);
asserts.isequal(e, 5);
asserts.isequal(m{1}, 'x aa');
asserts.isequal(regexp(['a', char(10), 'b', char(10)], 'b\s^', 'lineanchors'), []);
asserts.isequal(regexp(['123', char(10), 'abcabcabcabc', char(10)], '^.{9}abc.*\n', 'lineanchors'), 5);
%=============================================================================
[s, e, m] = regexp('世界您好', '您好$', 'start', 'end', 'match', 'once');
asserts.isequal(m, '您好');
[s, e, m] = regexp('世界您好', '^世界', 'start', 'end', 'match', 'once');
asserts.isequal(m, '世界');
asserts.isequal(regexp('世界您好', '世界$'), []);
asserts.isequal(regexp('世界您好', '^您好'), []);
[s, e, m] = regexp('世界您好', '界您', 'start', 'end', 'match', 'once');
asserts.isequal(m, '界您');
asserts.isequal(regexp('世界您好', '界_您'), []);
%=============================================================================
[a, b, c, piStringSplit] = regexp('3.14', '(\d+)\.(\d+)', 'start', 'end', 'match', 'tokens', 'once');
asserts.isequal(piStringSplit{1}, '3');
asserts.isequal(piStringSplit{2}, '14');
[a, b, c, d] = regexp('xabyabbbz', 'ab(.*)b(.*)', 'start', 'end', 'match', 'tokens', 'once');
asserts.isequal(size(d), [1, 2]);
[a, b, c, d] = regexp('https://www.nelson.org/download/', '^(?:https://)?([^/]+)', 'start', 'end', 'match', 'tokens', 'once', 'ignorecase');
asserts.isequal(d{1}, 'www.nelson.org');
[a, b, c, d] = regexp('foobar: 2025', '(?<name>\w+): (?<digit>\d+)', 'start', 'end', 'match', 'tokens', 'once');
asserts.isequal(d{1}, 'foobar');
asserts.isequal(d{2}, '2025');
%=============================================================================
% a scalar (1x1) cell input yields a 1x1 cell output (not an unwrapped result)
r = regexp({'a1b2'}, '\d', 'match');
asserts.isequal(iscell(r), true);
asserts.isequal(size(r), [1 1]);
asserts.isequal(r{1}, {'1', '2'});
%=============================================================================
% POSIX word-boundary anchors \< (start of word) and \> (end of word)
r = regexp('cat cats', '\<cat\>', 'match');
asserts.isequal(r, {'cat'});
r = regexp('cat scatter', '\<cat', 'match');
asserts.isequal(r, {'cat'});
r = regexp('cat scat', 'cat\>', 'match');
asserts.isequal(r, {'cat', 'cat'});
% an escaped literal backslash before '<' is not treated as a word anchor
r = regexp('a\<b', '\\<', 'match');
asserts.isequal(r, {'\<'});
% '<' inside a character class stays literal
r = regexp('a<b', '[<]', 'match');
asserts.isequal(r, {'<'});
%=============================================================================
% Unbalanced metacharacters parse as literals, not errors.
% A lone '(' matches the literal parenthesis (as in version() strings).
asserts.isequal(regexp('9.14 (R2023a)', '('), 6);
% A lone ')' matches the literal closing parenthesis.
asserts.isequal(regexp('x)y', ')'), 2);
% A lone '[' (never terminated) matches a literal bracket.
asserts.isequal(regexp('a[b', '['), 2);
asserts.isequal(isempty(regexp('ab', '[')), true);
% multiple unbalanced parentheses each become literal
asserts.isequal(regexp('(a)', '('), 1);
asserts.isequal(regexp('a)b(c', ')'), 2);
% an unbalanced outer '(' is literal while a valid inner group still captures
r = regexp('(x', '(\w', 'match');
asserts.isequal(r, {'(x'});
% escaped metacharacters keep working (no leniency needed, exact behavior)
asserts.isequal(regexp('a(b', '\('), 2);
asserts.isequal(regexp('a)b', '\)'), 2);
% valid constructs are never altered by the leniency fallback
asserts.isequal(regexp('x', '(?<n>x)'), 1);
asserts.isequal(regexp('bat cat', 'c[aeiou]+t'), 5);
%=============================================================================
