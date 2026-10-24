%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('asserts.warning'), -2);
asserts.isequal(nargout('asserts.warning'), 2);
%=============================================================================
stBegin = warning();
warning('on');
warning('on', 'Nelson:asserts:test');
%=============================================================================
asserts.warning('warning(''Nelson:asserts:test'', ''expected warning'');', 'expected warning');
asserts.warning('warning(''Nelson:asserts:test'', ''expected warning'');', 'Nelson:asserts:test');
asserts.warning('warning(''Nelson:asserts:test'', ''expected warning'');', 'expected warning', 'Nelson:asserts:test');
%=============================================================================
[r, msg] = asserts.warning('1 + 1', 'expected warning');
asserts.isfalse(r);
asserts.isequal(msg, _('No warning was produced while evaluating command.'));
%=============================================================================
[r, msg] = asserts.warning('warning(''Nelson:asserts:test'', ''expected warning'');', 'other warning');
asserts.isfalse(r);
asserts.istrue(contains(msg, 'expected warning message or identifier'));
%=============================================================================
[r, msg] = asserts.warning('cos', 'unused');
asserts.isfalse(r);
asserts.istrue(contains(msg, _('Unexpected error was produced while evaluating command:')));
%=============================================================================
asserts.checkerror('feval(''asserts.warning'', ''warning(''''Nelson:asserts:test'''', ''''expected warning'''');'', '''')', _('empty string not allowed as expected warning message or identifier.'));
%=============================================================================
warning(stBegin);
%=============================================================================
