%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('strfind'), 2);
assert_isequal(nargout('strfind'), 1);
%=============================================================================
k = strfind('', '');
ref = [];
assert_isequal(k, ref);
%=============================================================================
k = strfind('', 'bb');
ref = [];
assert_isequal(k, ref);
%=============================================================================
k = strfind('aaaa', '');
ref = [];
assert_isequal(k, ref);
%=============================================================================
k = strfind({}, 'a');
ref = {};
assert_isequal(k, ref);
%=============================================================================
idx = strfind({'r'}, []);
ref = {[]};
assert_isequal(idx, ref);
%=============================================================================
S = 'Find the starting indices of the pattern string';
k = strfind(S, 'in');
ref = [2    15    19    45];
assert_isequal(k, ref);
%=============================================================================
S = 'Find the starting indices of the pattern string';
k = strfind(S, 'In');
ref = [];
assert_isequal(k, ref);
%=============================================================================
S = 'Find the starting indices of the pattern string';
k = strfind(S, ' ');
ref = [5     9    18    26    29    33    41];
assert_isequal(k, ref);
%=============================================================================
idx = strfind('aaaa', 5);
ref = [];
assert_isequal(idx, ref);
%=============================================================================
cstr = {'How much wood would a woodchuck chuck';
'if a woodchuck could chuck wood?'};
idx = strfind(cstr, 'wood');
ref = {[10 23]; [6 28]};
assert_isequal(idx, ref);
%=============================================================================
idx = strfind([], []);
ref = [];
assert_isequal(idx, ref);
%=============================================================================
idx = strfind({}, '');
ref = {};
assert_isequal(idx, ref);
%=============================================================================
idx = strfind('', '1');
ref = [];
assert_isequal(idx, ref);
%=============================================================================
idx = strfind('aaaa','aa');
ref = [ 1 2 3 ];
assert_isequal(idx, ref);
%=============================================================================
idx = strfind({'aaa', 'bbb'; 'aab', 'bba'; '', ''}, 'aa');
ref = cell(3, 2);
ref{1, 1} = [1, 2];
ref{2, 1} = 1;
assert_isequal(idx, ref);
%=============================================================================
idx = strfind(ones(1,3), []);
ref = [];
assert_isequal(idx, ref);
%=============================================================================
idx = strfind(5,'aa');
ref = [];
assert_isequal(idx, ref);
%=============================================================================
c1 = 'time';
c2 = {'Once','upon';
'a','time'};
assert_checkerror('r = strfind(c1, c2);', _('Second argument a single string expected.'));
assert_checkerror('idx = strfind(ones(3,0), []);', _('Input strings must have one row.'));
assert_checkerror('idx = strfind([], {});', _('Second argument a single string expected.'));
assert_checkerror('k = strfind(''aaa'', {});', _('Second argument a single string expected.'));
assert_checkerror('idx = strfind({}, {});', _('Second argument a single string expected.'));
assert_checkerror('idx = strfind('''', {''''});', _('Second argument a single string expected.'));
assert_checkerror('idx = strfind(''aaaa'',[''a'';''a'']);', _('Second argument a single string expected.'));
assert_checkerror('strfind([''aaaa'';''bbbb''],{''aba'',''aa''})', _('Wrong type for argument #1: string or cell expected.'))
assert_checkerror('strfind()', _('Wrong number of input arguments.'));
expected_msg = _("'ForceCellOutput' expected as third input argument.");
assert_checkerror('strfind(''aaaa'', ''aa'', ''overlaps'', -1)', expected_msg);
%=============================================================================
str = 'No pain no gain.';
k = strfind(str,'in','ForceCellOutput',true);
assert_isequal(iscell(k), true);
assert_isequal(k, {[6 14]});
%=============================================================================
k = strfind(str,'in','ForceCellOutput',false);
assert_isequal(iscell(k), false);
assert_isequal(k, [6 14]);
%=============================================================================
S = "Find the starting indices of the pattern string";
k = strfind(S, "in");
ref = [2    15    19    45];
assert_isequal(k, ref);
%=============================================================================
idx = strfind(5, "aa");
ref = [];
assert_isequal(idx, ref);
%=============================================================================
idx = strfind(["aaa", "bbb"; "aab", "bba"; "", NaN], "aa");
ref = cell(3, 2);
ref{1, 1} = [1, 2];
ref{2, 1} = 1;
assert_isequal(idx, ref);
%=============================================================================
