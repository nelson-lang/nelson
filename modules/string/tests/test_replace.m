%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('replace'), 3);
assert_isequal(nargout('replace'), 1);
%=============================================================================
r = replace('This is a string.', 'is', 'is not');
ref = 'This not is not a string.';
assert_isequal(r, ref);
%=============================================================================
r = replace('abc 4444 def 444 ghi 44 jkl 4', '4', '*');
ref = 'abc **** def *** ghi ** jkl *';
assert_isequal(r, ref);
%=============================================================================
r = replace('abc 4444 def 444 ghi 44 jkl 4', '44', '*');
ref =   'abc ** def *4 ghi * jkl 4';
assert_isequal(r, ref);
%=============================================================================
r = replace('bcbcbcd', 'bcbc', '123');
ref = '123bcd';
assert_isequal(r, ref);
%=============================================================================
r = replace('string1','r','a');
ref = 'staing1';
assert_isequal(r, ref);
%=============================================================================
r = replace('string1','tr','b');
ref = 'sbing1';
assert_isequal(r, ref);
%=============================================================================
r = replace('string1','st','cd');
ref = 'cdring1';
assert_isequal(r, ref);
%=============================================================================
r = replace('[40,50,60]','1','0');
ref = '[40,50,60]';
assert_isequal(r, ref);
%=============================================================================
r = replace('[10,20,30;40,50,60]','20','100');
ref =  '[10,100,30;40,50,60]';
assert_isequal(r, ref);
%=============================================================================
r = replace('','','');
ref = char([]);
assert_isequal(r, ref);
%=============================================================================
r = replace('ara','a','bb');
ref = 'bbrbb';
assert_isequal(r, ref);
%=============================================================================
r = replace('aa','a','');
ref = char([]);
assert_isequal(r, ref);
%=============================================================================
r = replace('cc','','b');
ref = 'bcbcb';
assert_isequal(r, ref);
%=============================================================================
r = replace({},'str','str');
ref = {};
assert_isequal(r, ref);
%=============================================================================
r = replace('str',{},'str');
ref = 'str';
assert_isequal(r, ref);
%=============================================================================
r = replace('str','','str');
ref = 'strsstrtstrrstr';
assert_isequal(r, ref);
%=============================================================================
r = replace('str',{''},'str');
ref =  'strsstrtstrrstr';
assert_isequal(r, ref);
%=============================================================================
r = replace({'str'},{''},'str');
ref =  {'strsstrtstrrstr'};
assert_isequal(r, ref);
%=============================================================================
r = replace({},{},{});
ref = {};
assert_isequal(r, ref);
%=============================================================================
r = replace('cccc','cc','b');
ref = 'bb';
assert_isequal(r, ref);
%=============================================================================
r = replace('cccc','cc','bbb');
ref = 'bbbbbb';
assert_isequal(r, ref);
%=============================================================================
r = replace({'cccc'},'cc','bb');
ref = {'bbbb'};
assert_isequal(r, ref);
%=============================================================================
r = replace({'cccc'},'cc',{'bb'});
ref = {'bbbb'};
assert_isequal(r, ref);
%=============================================================================
r = replace({'cccc'},{'cc'},'bb');
ref = {'bbbb'};
assert_isequal(r, ref);
%=============================================================================
r = replace('cccc',{'cc'},{'bb'});
ref = 'bbbb';
assert_isequal(r, ref);
%=============================================================================
r = replace({'cccc','ccbbcca'},'cc','bb');
ref = { 'bbbb'    'bbbbbba'};
assert_isequal(r, ref);
%=============================================================================
r = replace({'cccc','ccbbcca'},{'cc','bb'},'cc');
ref = {  'cccc'    'cccccca'};
assert_isequal(r, ref);
%=============================================================================
r = replace({'cccc','ccbbcca'},{'cc','bb'},{'cc'});
ref = {'cccc'    'cccccca'};
assert_isequal(r, ref);
%=============================================================================
str = {'the quick yellow canary';
'and the lazy bunny'};
r = replace(str,'the','a');
ref = {'a quick yellow canary';
'and a lazy bunny'};
assert_isequal(r, ref);
%=============================================================================
r = replace('cccc',{'cc'},'bb');
ref = 'bbbb';
assert_isequal(r, ref);
%=============================================================================
r = replace('cccc','cc',{'bb'});
ref = 'bbbb';
assert_isequal(r, ref);
%=============================================================================
r = replace('','','str');
ref = 'str';
assert_isequal(r, ref);
%=============================================================================
r = replace('cccc',{},'bb');
ref = 'cccc';
assert_isequal(r, ref);
%=============================================================================
r = replace({'cccc'},{},'bb');
ref = {'cccc'};
assert_isequal(r, ref);
%=============================================================================
OLD = {'ccabbba','bbccbb';
'abba','bccb'};
STR = {'cc';
'bb'};
NEW = {'cc';
'e'};
r = replace(OLD, STR, NEW);
ref = {'ccaeba'    'ecce';
'aea'       'bccb'};
assert_isequal(r, ref);
%=============================================================================
r = replace({'r','a'},{'a','r'},{'rrr'});
ref = {'r','a'};
assert_isequal(r, ref);
%=============================================================================
r = replace('ccabbba',{'cc','bb'},{'cc','e'});
ref =  'ccaeba';
assert_isequal(r, ref);
%=============================================================================
r = replace({'ccabbba'},{'cc','bb'},{'cc','e'});
ref =  {'ccaeba'};
assert_isequal(r, ref);
%=============================================================================
cmd = 'r = replace(''str'',''str'',[]);';
ref = _('Invalid input argument(s): cell or string expected.');
assert_checkerror(cmd, ref);
%=============================================================================
cmd = 'r = replace(3i,''str'',''str'');';
ref = _('Invalid input argument(s): cell or string expected.');
assert_checkerror(cmd, ref);
%=============================================================================
cmd = 'r = replace(''str'',''str'',3i);';
ref = _('Invalid input argument(s): cell or string expected.');
assert_checkerror(cmd, ref);
%=============================================================================
cmd = 'r = replace({''cccc'',''ccbbcca''},{''cc'',''bb''},{''cc'',''e'',''ddd''});';
ref = _('Same size expected.');
assert_checkerror(cmd, ref);
%=============================================================================
cmd = 'r = replace(''ccbbba'',{''cc'',''bb''},{''cc'',''e'',''ddd''});';
ref = _('Same size expected.');
assert_checkerror(cmd, ref);
%=============================================================================
cmd = 'r = replace({{''bbb''},''ccabbba''},{''cc'',''bb''},{{''cc'',''e'',''ddd''},''str''});';
ref = _('Invalid input argument(s): cell or string expected.');
assert_checkerror(cmd, ref);
%=============================================================================
cmd = 'r = replace(''ccabbba'',{''cc'',{},''bb''},{{''cc'',''e'',''ddd''}})';
ref = _('Invalid input argument(s): cell or string expected.');
assert_checkerror(cmd, ref);
%=============================================================================
cmd = 'r = replace(''cccc'',''cc'',{});';
ref = _('Same size expected.');
assert_checkerror(cmd, ref);
%=============================================================================
str = {'First Date: ___';
'Last Date: ___'};
old = '___';
new = {'1973-04-08';
'1974-15-08'};
cmd = 'r = replace(str, old, new);';
ref = _('Same size expected.');
assert_checkerror(cmd, ref);
%=============================================================================
cmd = 'r = replace(''str'',''str'',{});';
ref = _('Same size expected.');
assert_checkerror(cmd, ref);
%=============================================================================
cmd = 'r = replace(''cccc'', ''cc'', {''bb'',''cc''});';
ref = _('Same size expected.');
assert_checkerror(cmd, ref);
%=============================================================================
cmd = 'r = replace(''cccc'',''cc'', {''a'',''bb''})';
ref = _('Same size expected.');
assert_checkerror(cmd, ref);
%=============================================================================
r = replace("This is a string.", 'is', 'is not');
ref = "This not is not a string.";
assert_isequal(r, ref);
%=============================================================================
r = replace('This is a string.', "is", 'is not');
ref = 'This not is not a string.';
assert_isequal(r, ref);
%=============================================================================
r = replace('This is a string.', 'is', "is not");
ref = 'This not is not a string.';
assert_isequal(r, ref);
%=============================================================================
r = replace("ccabbba",{'cc','bb'},{'cc','e'});
ref =  "ccaeba";
assert_isequal(r, ref);
%=============================================================================
str = ["the quick yellow canary";
"and the lazy bunny"];
r = replace(str,'the','a');
ref = ["a quick yellow canary";
"and a lazy bunny"];
assert_isequal(r, ref);
%=============================================================================
