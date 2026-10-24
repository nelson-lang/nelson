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
p = digitsPattern(2);
R = extract('a12b345', p);
asserts.isequal(R{1}, '12');
asserts.isequal(char(lettersPattern()), '(?:[A-Za-z])+');
asserts.isequal(char(alphanumericsPattern()), '(?:[A-Za-z0-9])+');
asserts.isequal(char(whitespacePattern()), '[\s\x{00A0}\x{2000}-\x{200A}]+');
asserts.isequal(char(characterListPattern('abc')), '[abc]');
asserts.isequal(char(regexpPattern('\d+')), '\d+');
asserts.isequal(char("a" | "b"), '(?:a)|(?:b)');
asserts.isequal(char("id" + digitsPattern(2)), '(?:id)(?:(?:\d){2})');
asserts.isequal(char(alphanumericsPattern(2, 4)), '(?:[A-Za-z0-9]){2,4}');
asserts.isequal(char(wildcardPattern("Except", "\")), '(?:(?:(?!\\).))*?');
asserts.isequal(char(characterListPattern("a", "g")), '[a-g]');
asserts.isequal(char(alphanumericBoundary("start")), '(?<!\w)(?=\w)');
asserts.isequal(char(digitBoundary("end")), '(?<=\d)(?!\d)');
asserts.isequal(char(regexpPattern('x', "IgnoreCase", true)), '(?i:x)');
asserts.istrue(isa(optionalPattern('a'), 'pattern'));
asserts.istrue(isa(possessivePattern('a'), 'pattern'));
asserts.istrue(isa(caseInsensitivePattern('a'), 'pattern'));
asserts.istrue(isa(caseSensitivePattern('a'), 'pattern'));
asserts.istrue(isa(asFewOfPattern('a'), 'pattern'));
asserts.istrue(isa(asManyOfPattern('a'), 'pattern'));
asserts.istrue(isa(alphanumericBoundary(), 'pattern'));
asserts.istrue(isa(digitBoundary(), 'pattern'));
asserts.istrue(isa(letterBoundary(), 'pattern'));
asserts.istrue(isa(whitespaceBoundary(), 'pattern'));
asserts.istrue(isa(lineBoundary(), 'pattern'));
asserts.istrue(isa(textBoundary(), 'pattern'));
asserts.istrue(isa(lookAheadBoundary('a'), 'pattern'));
asserts.istrue(isa(lookBehindBoundary('a'), 'pattern'));
asserts.istrue(isa(maskedPattern('a', 'letter a'), 'pattern'));
asserts.istrue(isa(namedPattern('a', 'letterA'), 'pattern'));
asserts.isequal(extract("abc 123 =+-*/", alphanumericsPattern), ["abc"; "123"]);
asserts.isequal(extract("The 2 parties agreed.", lettersPattern), ["The"; "parties"; "agreed"]);
asserts.isequal(extract("The answer is 0x2A", "0x" + asManyOfPattern(caseInsensitivePattern(characterListPattern('0', '9') | characterListPattern('a', 'f')), 1)), "0x2A");
asserts.isequal(replace("123 abc .?!", ~digitBoundary, "|"), "1|2|3 |a|b|c| |.|?|!|");
txt = "This is line one." + newline + "Here is line two.";
asserts.isequal(extract(txt, lineBoundary + lettersPattern), ["This"; "Here"]);
emailPattern = namedPattern(namedPattern(lettersPattern) + "@" + namedPattern(lettersPattern));
emailPattern.emailAddress.domain = "example.com";
asserts.istrue(isa(emailPattern, 'pattern'));
%=============================================================================
