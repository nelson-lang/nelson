%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('asserts.noError'), 1);
asserts.isequal(nargout('asserts.noError'), 2);
%=============================================================================
asserts.noError('1 + 1');
%=============================================================================
[r, msg] = asserts.noError('cos');
asserts.isfalse(r);
asserts.istrue(contains(msg, _('Unexpected error was produced while evaluating command:')));
%=============================================================================
expected = ['Unexpected error was produced while evaluating command: ', _('Not enough input arguments.')];
asserts.checkerror('feval(''asserts.noError'', ''cos'')', expected);
%=============================================================================
