%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('strrep'), 3);
assert_isequal(nargout('strrep'), 1);
%=============================================================================
r = strrep('This is a string.', 'is', 'is not');
ref = 'This not is not a string.';
assert_isequal(r, ref);
%=============================================================================
r = strrep('abc 4444 def 444 ghi 44 jkl 4', '4', '*');
ref = 'abc **** def *** ghi ** jkl *';
assert_isequal(r, ref);
%=============================================================================
r = strrep('abc 4444 def 444 ghi 44 jkl 4', '44', '*');
ref =  'abc *** def ** ghi * jkl 4';
assert_isequal(r, ref);
%=============================================================================
r = strrep('bcbcbcd', 'bcbc', '123');
ref = '123123d';
assert_isequal(r, ref);
%=============================================================================
r = strrep('string1','r','a');
ref = 'staing1';
assert_isequal(r, ref);
%=============================================================================
r = strrep('string1','tr','b');
ref = 'sbing1';
assert_isequal(r, ref);
%=============================================================================
r = strrep('string1','st','cd');
ref = 'cdring1';
assert_isequal(r, ref);
%=============================================================================
r = strrep('[4,5,6]','1','0');
ref = '[4,5,6]';
assert_isequal(r, ref);
%=============================================================================
r = strrep('[10,20,30;40,50,60]','20','100');
ref =  '[10,100,30;40,50,60]';
assert_isequal(r, ref);
%=============================================================================
r = strrep('','','');
ref = char([]);
assert_isequal(r, ref);
%=============================================================================
r = strrep('ara','a','bb');
ref = 'bbrbb';
assert_isequal(r, ref);
%=============================================================================
r = strrep('aa','a','');
ref = char([]);
assert_isequal(r, ref);
%=============================================================================
r = strrep('cc','','b');
ref = 'cc';
assert_isequal(r, ref);
%=============================================================================
r = strrep({},'str','str');
ref = {};
assert_isequal(r, ref);
%=============================================================================
r = strrep('str',{},'str');
ref = {};
assert_isequal(r, ref);
%=============================================================================
r = strrep('str','str', {});
ref = {};
assert_isequal(r, ref);
%=============================================================================
r = strrep('str','','str');
ref = 'str';
assert_isequal(r, ref);
%=============================================================================
r = strrep('str',{''},'str');
ref = {'str'};
assert_isequal(r, ref);
%=============================================================================
r = strrep('str','str',{});
ref = {};
assert_isequal(r, ref);
%=============================================================================
r = strrep({},{},{});
ref = {};
assert_isequal(r, ref);
%=============================================================================
r = strrep('cccc','cc','b');
ref = 'bbb';
assert_isequal(r, ref);
%=============================================================================
r = strrep('cccc','cc','bbb');
ref = 'bbbbbbbbb';
assert_isequal(r, ref);
%=============================================================================
r = strrep({'r','a'},{'a','r'},{'rrr'});
ref = {'r','a'};
assert_isequal(r, ref);
%=============================================================================
r = strrep('cccc',{'cc'},'bb');
ref = {'bbbbbb'};
assert_isequal(r, ref);
%=============================================================================
r = strrep('cccc','cc',{'bb'});
ref = {'bbbbbb'};
assert_isequal(r, ref);
%=============================================================================
r = strrep({'cccc'},'cc','bb');
ref = {'bbbbbb'};
assert_isequal(r, ref);
%=============================================================================
r = strrep({'cccc'},'cc',{'bb'});
ref = {'bbbbbb'};
assert_isequal(r, ref);
%=============================================================================
r = strrep({'cccc'},{'cc'},'bb');
ref = {'bbbbbb'};
assert_isequal(r, ref);
%=============================================================================
r = strrep('cccc',{'cc'},{'bb'});
ref = {'bbbbbb'};
assert_isequal(r, ref);
%=============================================================================
r = strrep('cccc','cc',{'bb','cc'});
ref = { 'bbbbbb'    'cccccc'};
assert_isequal(r, ref);
%=============================================================================
r = strrep('cccc',{'a','cc'},'bb');
ref = {  'cccc'    'bbbbbb'};
assert_isequal(r, ref);
%=============================================================================
r = strrep({'cccc','ccbbcca'},'cc','bb');
ref = { 'bbbbbb'    'bbbbbba'};
assert_isequal(r, ref);
%=============================================================================
r = strrep({'cccc','ccbbcca'},{'cc','bb'},'cc');
ref = {  'cccccc'    'cccccca'};
assert_isequal(r, ref);
%=============================================================================
r = strrep({'cccc','ccbbcca'},{'cc','bb'},{'cc'});
ref = { 'cccccc'    'cccccca'};
assert_isequal(r, ref);
%=============================================================================
r = strrep('ccabbba',{'cc','bb'},{'cc','e'});
ref = {  'ccabbba'    'ccaeea'};
assert_isequal(r, ref);
%=============================================================================
str = {'the quick yellow canary';
'and the lazy bunny'};
r = strrep(str,'the','a');
ref = {'a quick yellow canary';
'and a lazy bunny'};
assert_isequal(r, ref);
%=============================================================================
str = {'First Date: ___';
'Last Date: ___'};
old = '___';
new = {'1973-04-08';
'1974-15-08'};
r = strrep(str, old, new);
ref = {   'First Date: 1973-04-08';
'Last Date: 1974-15-08'};
assert_isequal(r, ref);
%=============================================================================
cmd = 'r = strrep(''str'',''str'',[]);';
ref = _('Invalid input argument(s): cell or string expected.');
assert_checkerror(cmd, ref);
%=============================================================================
cmd = 'r = strrep(3i,''str'',''str'');';
ref = _('Invalid input argument(s): cell or string expected.');
assert_checkerror(cmd, ref);
%=============================================================================
cmd = 'r = strrep(''str'',''str'',3i);';
ref = _('Invalid input argument(s): cell or string expected.');
assert_checkerror(cmd, ref);
%=============================================================================
cmd = 'r = strrep({''cccc'',''ccbbcca''},{''cc'',''bb''},{''cc'',''e'',''ddd''});';
ref = _('Same size expected.');
assert_checkerror(cmd, ref);
%=============================================================================
cmd = 'r = strrep(''ccbbba'',{''cc'',''bb''},{''cc'',''e'',''ddd''});';
ref = _('Same size expected.');
assert_checkerror(cmd, ref);
%=============================================================================
cmd = 'r = strrep({{''bbb''},''ccabbba''},{''cc'',''bb''},{{''cc'',''e'',''ddd''},''str''});';
ref = _('Invalid input argument(s): cell or string expected.');
assert_checkerror(cmd, ref);
%=============================================================================
cmd = 'r = strrep(''ccabbba'',{''cc'',{},''bb''},{{''cc'',''e'',''ddd''}})';
ref = _('Invalid input argument(s): cell or string expected.');
assert_checkerror(cmd, ref);
%=============================================================================
cmd = 'r = strrep({''ccabbba'',''bbccbb'';''abba'',''bccb''},{''cc'';''bb''},{''cc'';''e''});';
ref = _('Same size expected.');
assert_checkerror(cmd, ref);
%=============================================================================
r = strrep("This is a string.", 'is', 'is not');
ref = "This not is not a string.";
assert_isequal(r, ref);
%=============================================================================
str = ["First Date: ___";
"Last Date: ___"];
old = '___';
new = {'1973-04-08';
'1974-15-08'};
r = strrep(str, old, new);
ref = ["First Date: 1973-04-08";
"Last Date: 1974-15-08"];
assert_isequal(r, ref);
%=============================================================================
