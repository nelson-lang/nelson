%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('warning'), -1);
assert_isequal(nargout('warning'), 1);
%=============================================================================
warning('This is an Warning message');
[msg, id] = lastwarn();
assert_isequal(msg, 'This is an Warning message');
assert_isequal(id, '');
%=============================================================================
warning()
%=============================================================================
stBegin = warning();
%=============================================================================
st = warning();
assert_istrue(isstruct(st));
assert_istrue(length(st) >= 2);
F = fieldnames(st);
R = {'identifier'; 'state'};
assert_isequal(F, R);
%=============================================================================
warning('query');
%=============================================================================
st = warning('query');
assert_istrue(isstruct(st));
assert_istrue(length(st) >= 2);
F = fieldnames(st);
R = {'identifier'; 'state'};
assert_isequal(F, R);
%=============================================================================
warning('off');
R = warning('off');
assert_istrue(isstruct(R));
assert_istrue(length(R) == 1);
assert_isequal(R.identifier, 'all');
assert_isequal(R.state, 'off');
%=============================================================================
warning('on');
R = warning('on');
assert_istrue(isstruct(R));
assert_istrue(length(R) == 1);
assert_isequal(R.identifier, 'all');
assert_isequal(R.state, 'on');
%=============================================================================
lastwarn('')
a = warning('nelson:test', 'ok');
assert_isequal(a, '')
a = warning('nelson:test2', 'ok2');
assert_isequal(a, 'ok')
%=============================================================================
lastwarn('')
[a, b] = warning('nelson:test', 'ok');
assert_isequal(a, '')
assert_isequal(b, '')
[a, b] = warning('nelson:test2', 'ok2');
assert_isequal(a, 'ok');
assert_isequal(b, 'nelson:test');
%=============================================================================
warning(stBegin);
c = warning();
assert_isequal(stBegin, c);
assert_istrue(isstruct(st));
assert_istrue(length(st) >= 2);
F = fieldnames(st);
R = {'identifier'; 'state'};
assert_isequal(F, R);
%=============================================================================
R = warning('on', 'nelson:test');
clear REF
REF.identifier = 'nelson:test';
REF.state = 'on';
assert_isequal(R, REF);
%=============================================================================
msg = _('warning(''query'') does not require an second argument.');
assert_checkerror('warning(''query'', ''nelson'');', msg);
%=============================================================================
warning('on', 'Nelson:colon:array-as-scalar');
[1:2]:3;
msg = lastwarn();
assert_isequal(msg, _('Array used as scalar.'));
%=============================================================================
lastwarn('');
warning('off', 'Nelson:colon:array-as-scalar');
[1:2]:3;
msg = lastwarn();
assert_isequal(msg, '');
%=============================================================================
warning('aserror', 'Nelson:colon:array-as-scalar');
assert_checkerror('[1:3]:4', _('Array used as scalar.'));
%=============================================================================
warning('blabla...');
R = lastwarn();
assert_isequal(R, 'blabla...');
warning('');
R = lastwarn();
assert_isequal(R, '');
%=============================================================================
warning(stBegin);
%=============================================================================
