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
asserts.isequal(nargin('daugment'), -1);
asserts.isequal(nargout('daugment'), 2);
%=============================================================================
rng(8);
[dA, XA] = daugment([-1 -1], 2, 'linear', 'Display', 'off', 'AvoidDuplicates', true);
asserts.isequal(size(dA), [3 2]);
asserts.isequal(size(XA), [3 3]);
asserts.isequal(dA(1, :), [-1 -1]);
%=============================================================================
[dA, XA] = daugment([0 0], 2, 'quadratic', 'Bounds', [0 1; 0 1], 'Display', 'off', 'AvoidDuplicates', true);
asserts.isequal(size(dA), [3 2]);
asserts.isequal(size(XA), [3 5]);
%=============================================================================
asserts.checkerror('daugment()', _('Not enough input arguments.'));
asserts.checkerror('daugment([], 1)', sprintf(_('%s must be a nonempty numeric matrix.'), 'DCE'));
asserts.checkerror('daugment([1 2], 0)', sprintf(_('%s must be a positive integer scalar.'), 'MRUNS'));
asserts.checkerror('daugment([1 2], 1, ''linear'', ''Display'')', _('Name-value arguments must be paired.'));
asserts.checkerror('daugment([1 2], 1, ''linear'', ''Unknown'', 1)', _('Invalid name-value argument name.'));
%=============================================================================
