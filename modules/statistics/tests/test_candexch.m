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
asserts.isequal(nargin('candexch'), -1);
asserts.isequal(nargout('candexch'), 1);
%=============================================================================
C = [ones(4, 1) (0:3)'];
rlist = candexch(C, 2, 'InitialDesign', C([2 3], :), 'Display', 'off', 'AvoidDuplicates', true);
asserts.isequal(sort(rlist), [1; 4]);
%=============================================================================
rlist = candexch(C, 1, 'FixedRows', [1 -1], 'Display', 'off');
asserts.isequal(rlist, 4);
%=============================================================================
rng(7);
r1 = candexch(C, 2, 'Display', 'off', 'NumTries', 3, 'AvoidDuplicates', true);
rng(7);
r2 = candexch(C, 2, 'Display', 'off', 'NumTries', 3, 'AvoidDuplicates', true);
asserts.isequal(r1, r2);
asserts.isequal(sort(r1), [1; 4]);
%=============================================================================
C2 = [ones(5, 1) (0:4)' ((0:4)') .^ 2];
rlist = candexch(C2, 3, 'Display', 'off', 'AvoidDuplicates', true);
asserts.isequal(size(rlist), [3 1]);
asserts.isequal(length(unique(rlist)), 3);
%=============================================================================
opts = statset('candexch');
asserts.isequal(opts.UseParallel, false);
rlist = candexch(C, 2, 'Display', 'off', 'Options', opts);
asserts.isequal(size(rlist), [2 1]);
%=============================================================================
asserts.checkerror('candexch()', _('Not enough input arguments.'));
asserts.checkerror('candexch([], 2)', sprintf(_('%s must be a nonempty numeric matrix.'), 'C'));
asserts.checkerror('candexch(C, 0)', sprintf(_('%s must be a positive integer scalar.'), 'NROWS'));
asserts.checkerror('candexch(C, 2, ''Display'')', _('Name-value arguments must be paired.'));
asserts.checkerror('candexch(C, 2, ''Unknown'', 1)', _('Invalid name-value argument name.'));
asserts.checkerror('candexch(C, 2, ''Display'', ''bad'')', _('Display must be on or off.'));
asserts.checkerror('candexch(C, 2, ''AvoidDuplicates'', 2)', sprintf(_('%s must be a scalar logical value.'), 'AvoidDuplicates'));
asserts.checkerror('candexch(C, 2, ''MaxIterations'', 0)', sprintf(_('%s must be a positive integer scalar.'), 'MaxIterations'));
asserts.checkerror('candexch(C, 2, ''NumTries'', 0)', sprintf(_('%s must be a positive integer scalar.'), 'NumTries'));
asserts.checkerror('candexch(C, 5, ''AvoidDuplicates'', true)', _('NROWS must not exceed the number of candidate rows when AvoidDuplicates is true.'));
asserts.checkerror('candexch(C, 2, ''InitialDesign'', [1 2 3; 1 2 4])', sprintf(_('%s must have the same number of columns as C.'), 'InitialDesign'));
asserts.checkerror('candexch(C, 2, ''InitialDesign'', C(1, :), ''Display'', ''off'')', _('InitialDesign must have NROWS rows.'));
asserts.checkerror('candexch(C, 2, ''InitialDesign'', [1 10; 1 11], ''Display'', ''off'')', _('InitialDesign rows must be rows from C.'));
asserts.checkerror('candexch(C, 2, ''FixedRows'', [1 2 3])', sprintf(_('%s must have the same number of columns as C.'), 'FixedRows'));
asserts.checkerror('candexch(C, 2, ''Options'', 1)', _('Options must be a structure.'));
%=============================================================================
