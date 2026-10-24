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
asserts.isequal(nargin('randsample'), -1);
asserts.isequal(nargout('randsample'), 1);
%=============================================================================
rng(10);
y1 = randsample(10, 4);
rng(10);
y2 = randsample(10, 4);
asserts.isequal(y1, y2);
asserts.isequal(length(unique(y1)), 4);
asserts.istrue(all(y1 >= 1));
asserts.istrue(all(y1 <= 10));
%=============================================================================
y = randsample([10 20 30], 5, true, [0 0 1]);
asserts.isequal(y, 30 * ones(5, 1));
%=============================================================================
rng(3);
y = randsample([4; 5], 6, true);
asserts.isequal(size(y), [6 1]);
asserts.istrue(all((y == 4) | (y == 5)));
%=============================================================================
y = randsample('abcd', 2, false);
asserts.isequal(size(y), [2 1]);
asserts.istrue(all((y == 'a') | (y == 'b') | (y == 'c') | (y == 'd')));
%=============================================================================
asserts.checkerror('randsample()', _('Not enough input arguments.'));
asserts.checkerror('randsample(0, 1)', sprintf(_('%s must be a positive integer scalar.'), 'N'));
asserts.checkerror('randsample(3, 0)', sprintf(_('%s must be a positive integer scalar.'), 'K'));
asserts.checkerror('randsample([], 1)', _('Population must not be empty.'));
asserts.checkerror('randsample(3, 4)', _('K must not exceed the population size without replacement.'));
asserts.checkerror('randsample(3, 2, 2)', sprintf(_('%s must be a scalar logical value.'), 'Replacement'));
asserts.checkerror('randsample(3, 2, false, ones(3, 1))', _('Weighted sampling requires replacement.'));
asserts.checkerror('randsample(3, 2, true, ones(2, 1))', _('Weights must be a nonnegative vector with one value per population element.'));
asserts.checkerror('randsample(3, 2, true, [1 -1 1])', _('Weights must be a nonnegative vector with one value per population element.'));
%=============================================================================
