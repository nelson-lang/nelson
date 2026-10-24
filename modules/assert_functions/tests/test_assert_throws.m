%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('asserts.throws'), -2);
asserts.isequal(nargout('asserts.throws'), 2);
%=============================================================================
expected = _('Not enough input arguments.');
asserts.throws('cos', expected);
%=============================================================================
[r, msg] = asserts.throws('cos', expected);
asserts.istrue(r);
asserts.isequal(msg, '');
%=============================================================================
[r, msg] = asserts.throws('cos', 'not found');
asserts.isfalse(r);
asserts.istrue(contains(msg, 'expected error message to contain'));
%=============================================================================
[r, msg] = asserts.throws('1 + 1', 'unused');
asserts.isfalse(r);
asserts.isequal(msg, _('No error was produced while evaluating command.'));
%=============================================================================
asserts.throws('mustBeFinite(NaN)', _('Value must be finite.'), 'Nelson:validators:mustBeFinite');
%=============================================================================
asserts.checkerror('feval(''asserts.throws'', ''cos'', '''')', _('empty string not allowed as expected substring.'));
%=============================================================================
