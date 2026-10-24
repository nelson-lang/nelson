%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
asserts.isequal(nargin('x2fx'), -3);
asserts.isequal(nargout('x2fx'), 1);
%=============================================================================
X = [1 2; 3 4];
asserts.isequal(x2fx(X, 'constant'), [1; 1]);
asserts.isequal(x2fx(X, 'linear'), [1 1 2; 1 3 4]);
asserts.isequal(x2fx(X, 'interaction'), [1 1 2 2; 1 3 4 12]);
asserts.isequal(x2fx(X, 'quadratic'), [1 1 2 1 4; 1 3 4 9 16]);
%=============================================================================
D = x2fx(X, [1 0; 0 2; 1 1]);
asserts.isequal(D, [1 1 4 2; 1 3 16 12]);
D = x2fx(X, 2);
asserts.isequal(size(D), [2 6]);
%=============================================================================
asserts.checkerror('x2fx()', _('Not enough input arguments.'));
asserts.checkerror('x2fx({1}, ''linear'')', sprintf(_('%s must be a numeric matrix.'), 'X'));
asserts.checkerror('x2fx([1 2], ''bad'')', _('Invalid model specification.'));
asserts.checkerror('x2fx([1 2], 0)', sprintf(_('%s must be a positive integer scalar.'), 'Model degree'));
asserts.checkerror('x2fx([1 2], [1 0 0])', sprintf(_('Model matrix must have one column per %s.'), 'factor'));
%=============================================================================
