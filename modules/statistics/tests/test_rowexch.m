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
asserts.isequal(nargin('rowexch'), -1);
asserts.isequal(nargout('rowexch'), 2);
%=============================================================================
rng(5);
[dRE, X] = rowexch(2, 3, 'linear', 'Display', 'off', 'AvoidDuplicates', true);
asserts.isequal(size(dRE), [3 2]);
asserts.isequal(size(X), [3 3]);
asserts.isequal(length(unique(dRE, 'rows')), 3);
%=============================================================================
rng(6);
[dRE1, X1] = rowexch(2, 3, 'linear', 'Bounds', [0 1; -1 1], 'Display', 'off', 'AvoidDuplicates', true);
rng(6);
[dRE2, X2] = rowexch(2, 3, 'linear', 'Bounds', [0 1; -1 1], 'Display', 'off', 'AvoidDuplicates', true);
asserts.isequal(dRE1, dRE2);
asserts.isequal(X1, X2);
%=============================================================================
asserts.checkerror('rowexch()', _('Not enough input arguments.'));
asserts.checkerror('rowexch(0, 3)', sprintf(_('%s must be a positive integer scalar.'), 'NFACTORS'));
asserts.checkerror('rowexch(2, 0)', sprintf(_('%s must be a positive integer scalar.'), 'NRUNS'));
asserts.checkerror('rowexch(2, 3, ''linear'', ''Display'')', _('Name-value arguments must be paired.'));
asserts.checkerror('rowexch(2, 3, ''linear'', ''Unknown'', 1)', _('Invalid name-value argument name.'));
asserts.checkerror('rowexch(2, 5, ''linear'', ''AvoidDuplicates'', true)', _('NROWS must not exceed the number of candidate rows when AvoidDuplicates is true.'));
%=============================================================================
