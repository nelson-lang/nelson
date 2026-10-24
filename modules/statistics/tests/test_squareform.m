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
asserts.isequal(nargin('squareform'), 2);
asserts.isequal(nargout('squareform'), 1);
%=============================================================================
D = [1 2 3];
Z = squareform(D);
asserts.isequal(Z, [0 1 2; 1 0 3; 2 3 0]);
asserts.isequal(squareform(Z), D);
asserts.isequal(squareform(D'), Z);
%=============================================================================
asserts.isequal(squareform(D, 'tomatrix'), Z);
asserts.isequal(squareform(Z, 'tovector'), D);
asserts.isequal(squareform(D, 'tom'), Z);
asserts.isequal(squareform(Z, 'tov'), D);
%=============================================================================
asserts.isequal(squareform(zeros(1, 0)), 0);
asserts.isequal(squareform(0), zeros(2, 2));
%=============================================================================
asserts.checkerror('squareform()', _('Not enough input arguments.'));
asserts.checkerror('squareform({1})', sprintf(_('%s must be a real numeric vector or matrix.'), 'Input'));
asserts.checkerror('squareform([1 2])', _('Distance vector length must be a binomial coefficient.'));
asserts.checkerror('squareform([0 1; 2 0])', sprintf(_('%s must be symmetric.'), 'Distance matrix'));
asserts.checkerror('squareform([0 1 2; 1 0 3])', sprintf(_('%s must be square.'), 'Distance matrix'));
asserts.checkerror('squareform([1 2 3], 1)', sprintf(_('%s must be a character vector or string scalar.'), 'Direction'));
asserts.checkerror('squareform([1 2 3], ''bad'')', sprintf(_('Invalid %s value.'), 'direction'));
asserts.checkerror('squareform([0 1; 1 0], ''tomatrix'')', sprintf(_('%s must be a vector.'), 'Value'));
%=============================================================================
D = pdist([0 0; 1 0; 0 2]);
asserts.isapprox(squareform(D), [0 1 2; 1 0 sqrt(5); 2 sqrt(5) 0], 1e-12);
%=============================================================================
