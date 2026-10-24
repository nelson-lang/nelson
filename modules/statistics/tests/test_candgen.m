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
asserts.isequal(nargin('candgen'), -1);
asserts.isequal(nargout('candgen'), 2);
%=============================================================================
F = candgen(2);
asserts.isequal(F, [-1 -1; 1 -1; -1 1; 1 1]);
[F, C] = candgen(2, 'quadratic');
asserts.isequal(size(F), [4 2]);
asserts.isequal(size(C), [4 5]);
%=============================================================================
[F, C] = candgen(2, 'linear', 'Bounds', [0 1 2; -1 1 1]);
asserts.isequal(size(F), [6 2]);
asserts.isequal(size(C), [6 3]);
asserts.istrue(all(F(:, 1) >= 0));
%=============================================================================
asserts.checkerror('candgen()', _('Not enough input arguments.'));
asserts.checkerror('candgen(0)', sprintf(_('%s must be a positive integer scalar.'), 'NFACTORS'));
asserts.checkerror('candgen(2, ''linear'', ''Bounds'')', _('Name-value arguments must be paired.'));
asserts.checkerror('candgen(2, ''linear'', ''Unknown'', 1)', _('Invalid name-value argument name.'));
asserts.checkerror('candgen(2, ''linear'', ''Bounds'', [0 1])', _('Bounds must have one row per factor.'));
asserts.checkerror('candgen(2, ''linear'', ''Bounds'', [0; 1])', _('Bounds must contain at least two levels per factor.'));
%=============================================================================
