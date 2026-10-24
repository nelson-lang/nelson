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
asserts.isequal(nargin('replace'), 3);
asserts.isequal(nargout('replace'), 1);
%=============================================================================
r = replace('This is a string.', 'is', 'is not');
ref = 'This not is not a string.';
asserts.isequal(r, ref);
%=============================================================================
r = replace('abc 4444 def 444 ghi 44 jkl 4', '4', '*');
ref = 'abc **** def *** ghi ** jkl *';
asserts.isequal(r, ref);
%=============================================================================
r = replace('abc 4444 def 444 ghi 44 jkl 4', '44', '*');
ref = 'abc ** def *4 ghi * jkl 4';
asserts.isequal(r, ref);
%=============================================================================
r = replace('bcbcbcd', 'bcbc', '123');
ref = '123bcd';
asserts.isequal(r, ref);
%=============================================================================
r = replace('string1', 'r', 'a');
ref = 'staing1';
asserts.isequal(r, ref);
%=============================================================================
r = replace('string1', 'tr', 'b');
ref = 'sbing1';
asserts.isequal(r, ref);
%=============================================================================
r = replace('string1', 'st', 'cd');
ref = 'cdring1';
asserts.isequal(r, ref);
%=============================================================================
r = replace('[40,50,60]', '1', '0');
ref = '[40,50,60]';
asserts.isequal(r, ref);
%=============================================================================
r = replace('[10,20,30;40,50,60]', '20', '100');
ref = '[10,100,30;40,50,60]';
asserts.isequal(r, ref);
%=============================================================================
r = replace('', '', '');
ref = char([]);
asserts.isequal(r, ref);
%=============================================================================
r = replace('ara', 'a', 'bb');
ref = 'bbrbb';
asserts.isequal(r, ref);
%=============================================================================
r = replace('aa', 'a', '');
ref = char([]);
asserts.isequal(r, ref);
%=============================================================================
r = replace('cc', '', 'b');
ref = 'bcbcb';
asserts.isequal(r, ref);
%=============================================================================
r = replace({}, 'str', 'str');
ref = {};
asserts.isequal(r, ref);
%=============================================================================
r = replace('str', {}, 'str');
ref = 'str';
asserts.isequal(r, ref);
%=============================================================================
r = replace('str', '', 'str');
ref = 'strsstrtstrrstr';
asserts.isequal(r, ref);
%=============================================================================
r = replace('str', {''}, 'str');
ref = 'strsstrtstrrstr';
asserts.isequal(r, ref);
%=============================================================================
r = replace({'str'}, {''}, 'str');
ref = {'strsstrtstrrstr'};
asserts.isequal(r, ref);
%=============================================================================
r = replace({}, {}, {});
ref = {};
asserts.isequal(r, ref);
%=============================================================================
r = replace('cccc', 'cc', 'b');
ref = 'bb';
asserts.isequal(r, ref);
%=============================================================================
r = replace('cccc', 'cc', 'bbb');
ref = 'bbbbbb';
asserts.isequal(r, ref);
%=============================================================================
r = replace({'cccc'}, 'cc', 'bb');
ref = {'bbbb'};
asserts.isequal(r, ref);
%=============================================================================
r = replace({'cccc'}, 'cc', {'bb'});
ref = {'bbbb'};
asserts.isequal(r, ref);
%=============================================================================
r = replace({'cccc'}, {'cc'}, 'bb');
ref = {'bbbb'};
asserts.isequal(r, ref);
%=============================================================================
r = replace('cccc', {'cc'}, {'bb'});
ref = 'bbbb';
asserts.isequal(r, ref);
%=============================================================================
r = replace({'cccc', 'ccbbcca'}, 'cc', 'bb');
ref = {'bbbb' 'bbbbbba'};
asserts.isequal(r, ref);
%=============================================================================
r = replace({'cccc', 'ccbbcca'}, {'cc', 'bb'}, 'cc');
ref = {'cccc' 'cccccca'};
asserts.isequal(r, ref);
%=============================================================================
r = replace({'cccc', 'ccbbcca'}, {'cc', 'bb'}, {'cc'});
ref = {'cccc' 'cccccca'};
asserts.isequal(r, ref);
%=============================================================================
str = {'the quick yellow canary';
  'and the lazy bunny'};
r = replace(str, 'the', 'a');
ref = {'a quick yellow canary';
  'and a lazy bunny'};
asserts.isequal(r, ref);
%=============================================================================
r = replace('cccc', {'cc'}, 'bb');
ref = 'bbbb';
asserts.isequal(r, ref);
%=============================================================================
r = replace('cccc', 'cc', {'bb'});
ref = 'bbbb';
asserts.isequal(r, ref);
%=============================================================================
r = replace('', '', 'str');
ref = 'str';
asserts.isequal(r, ref);
%=============================================================================
r = replace('cccc', {}, 'bb');
ref = 'cccc';
asserts.isequal(r, ref);
%=============================================================================
r = replace({'cccc'}, {}, 'bb');
ref = {'cccc'};
asserts.isequal(r, ref);
%=============================================================================
OLD = {'ccabbba', 'bbccbb';
  'abba', 'bccb'};
STR = {'cc';
  'bb'};
NEW = {'cc';
  'e'};
r = replace(OLD, STR, NEW);
ref = {'ccaeba' 'ecce';
  'aea' 'bccb'};
asserts.isequal(r, ref);
%=============================================================================
r = replace({'r', 'a'}, {'a', 'r'}, {'rrr'});
ref = {'rrr', 'rrr'};
asserts.isequal(r, ref);
%=============================================================================
r = replace('ccabbba', {'cc', 'bb'}, {'cc', 'e'});
ref = 'ccaeba';
asserts.isequal(r, ref);
%=============================================================================
r = replace({'ccabbba'}, {'cc', 'bb'}, {'cc', 'e'});
ref = {'ccaeba'};
asserts.isequal(r, ref);
%=============================================================================
cmd = 'r = replace(''str'',''str'',[]);';
ref = _('Invalid input argument(s): cell or string expected.');
asserts.checkerror(cmd, ref);
%=============================================================================
cmd = 'r = replace(3i,''str'',''str'');';
ref = _('Invalid input argument(s): cell or string expected.');
asserts.checkerror(cmd, ref);
%=============================================================================
cmd = 'r = replace(''str'',''str'',3i);';
ref = _('Invalid input argument(s): cell or string expected.');
asserts.checkerror(cmd, ref);
%=============================================================================
cmd = 'r = replace({''cccc'',''ccbbcca''},{''cc'',''bb''},{''cc'',''e'',''ddd''});';
ref = _('Same size expected.');
asserts.checkerror(cmd, ref);
%=============================================================================
cmd = 'r = replace(''ccbbba'',{''cc'',''bb''},{''cc'',''e'',''ddd''});';
ref = _('Same size expected.');
asserts.checkerror(cmd, ref);
%=============================================================================
cmd = 'r = replace({{''bbb''},''ccabbba''},{''cc'',''bb''},{{''cc'',''e'',''ddd''},''str''});';
ref = _('Invalid input argument(s): cell or string expected.');
asserts.checkerror(cmd, ref);
%=============================================================================
cmd = 'r = replace(''ccabbba'',{''cc'',{},''bb''},{{''cc'',''e'',''ddd''}})';
ref = _('Invalid input argument(s): cell or string expected.');
asserts.checkerror(cmd, ref);
%=============================================================================
cmd = 'r = replace(''cccc'',''cc'',{});';
ref = _('Same size expected.');
asserts.checkerror(cmd, ref);
%=============================================================================
str = {'First Date: ___';
  'Last Date: ___'};
old = '___';
new = {'1973-04-08';
  '1974-15-08'};
cmd = 'r = replace(str, old, new);';
ref = _('Same size expected.');
asserts.checkerror(cmd, ref);
%=============================================================================
cmd = 'r = replace(''str'',''str'',{});';
ref = _('Same size expected.');
asserts.checkerror(cmd, ref);
%=============================================================================
cmd = 'r = replace(''cccc'', ''cc'', {''bb'',''cc''});';
ref = _('Same size expected.');
asserts.checkerror(cmd, ref);
%=============================================================================
cmd = 'r = replace(''cccc'',''cc'', {''a'',''bb''})';
ref = _('Same size expected.');
asserts.checkerror(cmd, ref);
%=============================================================================
r = replace("This is a string.", 'is', 'is not');
ref = "This not is not a string.";
asserts.isequal(r, ref);
%=============================================================================
r = replace('This is a string.', "is", 'is not');
ref = 'This not is not a string.';
asserts.isequal(r, ref);
%=============================================================================
r = replace('This is a string.', 'is', "is not");
ref = 'This not is not a string.';
asserts.isequal(r, ref);
%=============================================================================
r = replace("ccabbba", {'cc', 'bb'}, {'cc', 'e'});
ref = "ccaeba";
asserts.isequal(r, ref);
%=============================================================================
str = ["the quick yellow canary";
  "and the lazy bunny"];
r = replace(str, 'the', 'a');
ref = ["a quick yellow canary";
  "and a lazy bunny"];
asserts.isequal(r, ref);
%=============================================================================
s = ["a", missing, "c"];
r = replace(s, "a", "X");
asserts.isequal(ismissing(r), [false, true, false]);
asserts.isequal(r(1), "X");
asserts.isequal(r(3), "c");
%=============================================================================
r = replace(string(missing), "a", "X");
asserts.istrue(ismissing(r));
%=============================================================================
% all the patterns are searched at once, left to right; an empty pattern
% matches everywhere but right after a replaced text.
asserts.isequal(replace('ab', {'a', 'b'}, {'b', 'a'}), 'ba');
asserts.isequal(replace({'ab', 'ba'}, {'a', 'b'}, {'b', 'a'}), {'ba', 'ab'});
asserts.isequal(replace({'ab', 'ba'}, {'a', 'b'}, 'X'), {'XX', 'XX'});
asserts.isequal(replace({'ab', 'ba'}, {'a', 'b'}, {'X'}), {'XX', 'XX'});
asserts.isequal(replace(["ab", "ba"], ["a", "b"], ["b", "a"]), ["ba", "ab"]);
asserts.isequal(replace("ab", ["a", "b"], ["b", "a"]), "ba");
asserts.isequal(replace('abab', {'ab', 'a'}, {'X', 'Y'}), 'XX');
asserts.isequal(replace('abab', {'a', 'ab'}, {'Y', 'X'}), 'YbYb');
asserts.isequal(replace('aaaa', {'aa', 'aaa'}, {'X', 'Y'}), 'XX');
asserts.isequal(replace('str', {'', 's'}, {'x', 'y'}), 'xsxtxrx');
asserts.isequal(replace('str', {'s', ''}, {'y', 'x'}), 'ytxrx');
asserts.isequal(replace('str', {''}, 'str'), 'strsstrtstrrstr');
asserts.isequal(replace({'ab'; 'cd'}, {'a', 'c'}, {'1', '2'}), {'1b'; '2d'});
%=============================================================================
