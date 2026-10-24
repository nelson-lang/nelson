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
asserts.isequal(nargin('strfind'), 2);
asserts.isequal(nargout('strfind'), 1);
%=============================================================================
k = strfind('', '');
ref = [];
asserts.isequal(k, ref);
%=============================================================================
k = strfind('', 'bb');
ref = [];
asserts.isequal(k, ref);
%=============================================================================
k = strfind('aaaa', '');
ref = [];
asserts.isequal(k, ref);
%=============================================================================
k = strfind({}, 'a');
ref = {};
asserts.isequal(k, ref);
%=============================================================================
idx = strfind({'r'}, []);
ref = {[]};
asserts.isequal(idx, ref);
%=============================================================================
S = 'Find the starting indices of the pattern string';
k = strfind(S, 'in');
ref = [2 15 19 45];
asserts.isequal(k, ref);
%=============================================================================
S = 'Find the starting indices of the pattern string';
k = strfind(S, 'In');
ref = [];
asserts.isequal(k, ref);
%=============================================================================
S = 'Find the starting indices of the pattern string';
k = strfind(S, ' ');
ref = [5 9 18 26 29 33 41];
asserts.isequal(k, ref);
%=============================================================================
idx = strfind('aaaa', 5);
ref = [];
asserts.isequal(idx, ref);
%=============================================================================
cstr = {'How much wood would a woodchuck chuck';
  'if a woodchuck could chuck wood?'};
idx = strfind(cstr, 'wood');
ref = {[10 23]; [6 28]};
asserts.isequal(idx, ref);
%=============================================================================
idx = strfind([], []);
ref = [];
asserts.isequal(idx, ref);
%=============================================================================
idx = strfind({}, '');
ref = {};
asserts.isequal(idx, ref);
%=============================================================================
idx = strfind('', '1');
ref = [];
asserts.isequal(idx, ref);
%=============================================================================
idx = strfind('aaaa', 'aa');
ref = [1 2 3];
asserts.isequal(idx, ref);
%=============================================================================
idx = strfind({'aaa', 'bbb'; 'aab', 'bba'; '', ''}, 'aa');
ref = cell(3, 2);
ref{1, 1} = [1, 2];
ref{2, 1} = 1;
asserts.isequal(idx, ref);
%=============================================================================
idx = strfind(ones(1, 3), []);
ref = [];
asserts.isequal(idx, ref);
%=============================================================================
idx = strfind(5, 'aa');
ref = [];
asserts.isequal(idx, ref);
%=============================================================================
c1 = 'time';
c2 = {'Once', 'upon';
  'a', 'time'};
asserts.checkerror('r = strfind(c1, c2);', _('Second argument a single string expected.'));
asserts.checkerror('idx = strfind(ones(3,0), []);', _('Input strings must have one row.'));
asserts.checkerror('idx = strfind([], {});', _('Second argument a single string expected.'));
asserts.checkerror('k = strfind(''aaa'', {});', _('Second argument a single string expected.'));
asserts.checkerror('idx = strfind({}, {});', _('Second argument a single string expected.'));
asserts.isequal(strfind('', {''}), []);
asserts.checkerror('idx = strfind(''aaaa'',[''a'';''a'']);', _('Second argument a single string expected.'));
asserts.checkerror('strfind([''aaaa'';''bbbb''],{''aba'',''aa''})', sprintf(_('Wrong type for argument #%d: %s expected.'), 1, 'string or cell'))
asserts.checkerror('strfind()', _('Not enough input arguments.'));
expected_msg = _("'ForceCellOutput' expected as third input argument.");
asserts.checkerror('strfind(''aaaa'', ''aa'', ''overlaps'', -1)', expected_msg);
%=============================================================================
str = 'No pain no gain.';
k = strfind(str, 'in', 'ForceCellOutput', true);
asserts.isequal(iscell(k), true);
asserts.isequal(k, {[6 14]});
%=============================================================================
k = strfind(str, 'in', 'ForceCellOutput', false);
asserts.isequal(iscell(k), false);
asserts.isequal(k, [6 14]);
%=============================================================================
S = "Find the starting indices of the pattern string";
k = strfind(S, "in");
ref = [2 15 19 45];
asserts.isequal(k, ref);
%=============================================================================
idx = strfind(5, "aa");
ref = [];
asserts.isequal(idx, ref);
%=============================================================================
idx = strfind(["aaa", "bbb"; "aab", "bba"; "", NaN], "aa");
ref = cell(3, 2);
ref{1, 1} = [1, 2];
ref{2, 1} = 1;
asserts.isequal(idx, ref);
%=============================================================================
