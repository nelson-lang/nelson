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
asserts.isequal(nargin('cordexch'), -1);
asserts.isequal(nargout('cordexch'), 2);
%=============================================================================
rng(7);
[dCE, X] = cordexch(2, 3, 'linear', 'Display', 'off', 'AvoidDuplicates', true);
asserts.isequal(size(dCE), [3 2]);
asserts.isequal(size(X), [3 3]);
asserts.isequal(length(unique(dCE, 'rows')), 3);
%=============================================================================
asserts.checkerror('cordexch()', _('Not enough input arguments.'));
asserts.checkerror('cordexch(0, 3)', sprintf(_('%s must be a positive integer scalar.'), 'NFACTORS'));
asserts.checkerror('cordexch(2, 0)', sprintf(_('%s must be a positive integer scalar.'), 'NRUNS'));
asserts.checkerror('cordexch(2, 3, ''linear'', ''Display'')', _('Name-value arguments must be paired.'));
asserts.checkerror('cordexch(2, 3, ''linear'', ''Unknown'', 1)', _('Invalid name-value argument name.'));
%=============================================================================
