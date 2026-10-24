%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 16-->
names = ["alpha.txt", "beta.csv"; "ALPHA.dat", "report.txt"];
asserts.isequal(contains(names, "alpha", 'IgnoreCase', true), logical([1 0; 1 0]));
asserts.isequal(startsWith(names, ["alpha"; "ALPHA"]), logical([1 0; 1 0]));
asserts.isequal(endsWith(names, ".txt"), logical([1 0; 0 1]));
asserts.isequal(matches(["abc", "123"], regexpPattern('\d+')), logical([0 1]));
%=============================================================================
asserts.isequal(replace("abcabc", "ab", "x"), "xcxc");
asserts.isequal(erase(["aba", "bbb"; "ccc", "abc"], "b"), ["aa", ""; "ccc", "ac"]);
asserts.isequal(extract("abc", 2), "b");
asserts.isequal(extractBefore(["a:b"; "c:d"], ":"), ["a"; "c"]);
asserts.isequal(extractAfter(["a:b"; "c:d"], ":"), ["b"; "d"]);
asserts.isequal(extractBetween("a[bc]d", "[", "]"), "bc");
asserts.isequal(replaceBetween("a[bc]d", "[", "]", "XX"), "a[XX]d");
asserts.isequal(eraseBetween("a[bc]d", "[", "]"), "a[]d");
asserts.isequal(insertBefore("a=b", "=", "XX"), "aXX=b");
asserts.isequal(insertAfter("a=b", "=", "XX"), "a=XXb");
%=============================================================================
asserts.isequal(split(["a,b"; "c,d"], ","), ["a", "b"; "c", "d"]);
asserts.isequal(split("", ","), "");
asserts.isequal(splitlines("a" + newline() + "b"), ["a"; "b"]);
asserts.isequal(join(["a"; "b"; "c"], ","), "a,b,c");
asserts.isequal(join(["a", string(NaN), "b"], "-"), string(NaN));
asserts.isequal(strsplit('a,b;c', {',', ';'}), {'a', 'b', 'c'});
asserts.isequal(strjoin({'a'; 'b'}, '|'), 'a|b');
%=============================================================================
[parts, delimiters] = strsplit('a1b22c', '\d+', 'DelimiterType', 'RegularExpression');
asserts.isequal(parts, {'a', 'b', 'c'});
asserts.isequal(delimiters, {'1', '22'});
[startIndex, endIndex, matchText] = regexp('aaa aab', 'aa', 'start', 'end', 'match', 'once');
asserts.isequal(startIndex, 1);
asserts.isequal(endIndex, 2);
asserts.isequal(matchText, 'aa');
[startIndex, endIndex, tokens, matchText, splitText, names] = regexp('ab12 cd34', ...
  '(?<word>[a-z]+)(?<num>\d+)', 'start', 'end', 'tokens', 'match', 'split', 'names');
asserts.isequal(startIndex, [1 6]);
asserts.isequal(endIndex, [4 9]);
asserts.isequal(tokens, {{'ab', '12'}, {'cd', '34'}});
asserts.isequal(matchText, {'ab12', 'cd34'});
asserts.isequal(splitText, {'', ' ', ''});
asserts.isequal({names.word}, {'ab', 'cd'});
asserts.isequal({names.num}, {'12', '34'});
asserts.isequal(regexprep('a1 b2', '(\w)(\d)', '$2-$1'), '1-a 2-b');
asserts.isequal(regexptranslate('escape', 'a.b'), 'a\.b');
%=============================================================================
[convertedA, convertedB] = convertStringsToChars("a", ["b"; "c"]);
asserts.isequal(convertedA, 'a');
asserts.isequal(convertedB, {'b'; 'c'});
contained = convertContainedStringsToChars({"a", struct('b', "c")});
asserts.isequal(contained{1}, 'a');
asserts.isequal(contained{2}.b, 'c');
asserts.isequal(convertCharsToStrings({'a', 'b'}), ["a", "b"]);
asserts.checkerror('split(missing, ",")', sprintf(_('Wrong type for argument #%d: %s expected.'), 1, 'string or cell'));
asserts.checkerror('matches(1, ''a'')', ...
  sprintf(_('Wrong type for argument #%d: %s expected.'), 1, 'string array or character vector or cell array of character vectors'));
asserts.checkerror('regexp(''abc'', 1)', sprintf(_('Wrong type for argument #%d: %s expected.'), 1, 'string or cell'));
%=============================================================================
asserts.isequal(extract("abc 123 =+-*/", alphanumericsPattern), ["abc"; "123"]);
asserts.isequal(extract("The 2 parties agreed.", lettersPattern), ["The"; "parties"; "agreed"]);
asserts.isequal(extract("a b", whitespacePattern), " ");
asserts.isequal(extract("a12b345", digitsPattern(2, 3)), ["12"; "345"]);
asserts.isequal(extract("abc123", lettersPattern(3) + lookAheadBoundary(digitsPattern(3))), "abc");
asserts.isequal(extract("abc123", lookBehindBoundary("abc") + digitsPattern(3)), "123");
asserts.isequal(extract("abc", textBoundary("start") + lettersPattern(3) + textBoundary("end")), "abc");
multiLineText = "This line" + newline() + "Here too";
asserts.isequal(extract(multiLineText, lineBoundary + lettersPattern), ["This"; "Here"]);
asserts.isequal(matches(["color"; "colour"], "colo" + optionalPattern("u") + "r"), logical([1; 1]));
asserts.isequal(contains(["Abc"; "xyz"], caseInsensitivePattern("abc")), logical([1; 0]));
asserts.isequal(contains(["Abc"; "abc"], caseSensitivePattern("abc")), logical([0; 1]));
asserts.isequal(extract("file.txt", wildcardPattern + "." + lettersPattern(3)), "file.txt");
asserts.istrue(isa(maskedPattern(digitsPattern(3), "code"), 'pattern'));
asserts.istrue(isa(namedPattern(lettersPattern, "word"), 'pattern'));
%=============================================================================
