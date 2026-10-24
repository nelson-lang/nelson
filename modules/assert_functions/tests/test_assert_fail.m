%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('asserts.fail'), -1);
asserts.isequal(nargout('asserts.fail'), 2);
%=============================================================================
[r, msg] = asserts.fail('forced failure');
asserts.isfalse(r);
asserts.isequal(msg, 'forced failure');
%=============================================================================
asserts.checkerror('feval(''asserts.fail'', ''forced failure'')', 'forced failure');
asserts.checkerror('feval(''asserts.fail'', ''forced failure'')', 'forced failure', 'Nelson:asserts:assertionFailed');
%=============================================================================
