%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('warning'), -1);
asserts.isequal(nargout('warning'), 1);
%=============================================================================
warning('This is an Warning message');
[msg, id] = lastwarn();
asserts.isequal(msg, 'This is an Warning message');
asserts.isequal(id, '');
%=============================================================================
warning()
%=============================================================================
stBegin = warning();
%=============================================================================
st = warning();
asserts.istrue(isstruct(st));
asserts.istrue(length(st) >= 2);
F = fieldnames(st);
R = {'identifier'; 'state'};
asserts.isequal(F, R);
%=============================================================================
warning('query');
%=============================================================================
st = warning('query');
asserts.istrue(isstruct(st));
asserts.istrue(length(st) >= 2);
F = fieldnames(st);
R = {'identifier'; 'state'};
asserts.isequal(F, R);
%=============================================================================
warning('off');
R = warning('off');
asserts.istrue(isstruct(R));
asserts.istrue(length(R) == 1);
asserts.isequal(R.identifier, 'all');
asserts.isequal(R.state, 'off');
%=============================================================================
warning('on');
R = warning('on');
asserts.istrue(isstruct(R));
asserts.istrue(length(R) == 1);
asserts.isequal(R.identifier, 'all');
asserts.isequal(R.state, 'on');
%=============================================================================
lastwarn('')
a = warning('nelson:test', 'ok');
asserts.isequal(a, '')
a = warning('nelson:test2', 'ok2');
asserts.isequal(a, 'ok')
%=============================================================================
lastwarn('')
[a, b] = warning('nelson:test', 'ok');
asserts.isequal(a, '')
asserts.isequal(b, '')
[a, b] = warning('nelson:test2', 'ok2');
asserts.isequal(a, 'ok');
asserts.isequal(b, 'nelson:test');
%=============================================================================
warning(stBegin);
c = warning();
asserts.isequal(stBegin, c);
asserts.istrue(isstruct(st));
asserts.istrue(length(st) >= 2);
F = fieldnames(st);
R = {'identifier'; 'state'};
asserts.isequal(F, R);
%=============================================================================
R = warning('on', 'nelson:test');
clear REF
REF.identifier = 'nelson:test';
REF.state = 'on';
asserts.isequal(R, REF);
%=============================================================================
% warning('query', ID) reports the state of a specific warning identifier
warning('off', 'nelson:queryTest');
clear R
R = warning('query', 'nelson:queryTest');
clear REF
REF.identifier = 'nelson:queryTest';
REF.state = 'off';
asserts.isequal(R, REF);
warning('on', 'nelson:queryTest');
R = warning('query', 'nelson:queryTest');
asserts.isequal(R.state, 'on');
% an unknown identifier is enabled by default
R = warning('query', 'nelson:neverSeen');
asserts.isequal(R.state, 'on');
%=============================================================================
warning('on', 'nelson:test:state');
warning('nelson:test:state', 'state warning');
msg = lastwarn();
asserts.isequal(msg, 'state warning');
%=============================================================================
lastwarn('');
warning('off', 'nelson:test:state');
warning('nelson:test:state', 'state warning');
msg = lastwarn();
% A disabled warning is not displayed but is still recorded by lastwarn.
asserts.isequal(msg, 'state warning');
%=============================================================================
warning('aserror', 'nelson:test:state');
asserts.checkerror('warning(''nelson:test:state'', ''state warning'');', 'state warning');
%=============================================================================
warning('blabla...');
R = lastwarn();
asserts.isequal(R, 'blabla...');
warning('');
R = lastwarn();
asserts.isequal(R, '');
%=============================================================================
warning(stBegin);
%=============================================================================
