%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--REUSE PROCESS-->
%=============================================================================
asserts.isequal(nargin('filloutliers'), -3);
asserts.isequal(nargout('filloutliers'), 5);
%=============================================================================
A = [57 59 60 100 59 58 57 58 300 61 62 60 62 58 57];
[B, TF] = filloutliers(A, 'linear');
asserts.isequal(TF, logical([0 0 0 1 0 0 0 0 1 0 0 0 0 0 0]));
asserts.isequal(B, [57 59 60 59.5 59 58 57 58 59.5 61 62 60 62 58 57]);
%=============================================================================
B = filloutliers(A, 'nearest', 'mean');
asserts.isequal(B, [57 59 60 100 59 58 57 58 58 61 62 60 62 58 57]);
B = filloutliers(A, 'previous');
asserts.isequal(B(4), 60);
asserts.isequal(B(9), 58);
B = filloutliers(A, 'next');
asserts.isequal(B(4), 59);
asserts.isequal(B(9), 61);
%=============================================================================
[B, TF, L, U, C] = filloutliers([60 59 49 49 58 100 61 57 48 58], 'clip');
asserts.isequal(TF, logical([0 0 0 0 0 1 0 0 0 0]));
asserts.isapprox(B(6), 69.119516638792021, 1e-12);
asserts.isapprox(L, 46.880483361207986, 1e-12);
asserts.isapprox(U, 69.119516638792021, 1e-12);
asserts.isequal(C, 58);
%=============================================================================
M = [1 2 100 4 5; 10 11 12 13 100];
[B, TF] = filloutliers(M, 0, 2);
asserts.isequal(B, [1 2 0 4 5; 10 11 12 13 0]);
asserts.isequal(TF, logical([0 0 1 0 0; 0 0 0 0 1]));
%=============================================================================
mask = logical([0 1 0 0 0]);
B = filloutliers([1 100 3 4 5], 'linear', 'OutlierLocations', mask);
asserts.isequal(B, [1 2 3 4 5]);
%=============================================================================
P = [1 1 100 1 1];
B = filloutliers(P, 'linear', 'movmedian', 3, 'SamplePoints', [1 2 100 101 102]);
asserts.isequal(B, P);
B = filloutliers(P, 'linear', 'movmedian', 3);
asserts.isequal(B, [1 1 1 1 1]);
%=============================================================================
D = reshape(single([1 2 100 4 5 6 7 80 9 10 11 12]), [3 2 2]);
[B, TF] = filloutliers(D, single(0), 'median', 1, 'ThresholdFactor', 2);
asserts.isequal(class(B), 'single');
asserts.isequal(size(B), [3 2 2]);
asserts.istrue(TF(3, 1, 1));
asserts.isequal(B(3, 1, 1), single(0));
%=============================================================================
asserts.checkerror('filloutliers()', _('Not enough input arguments.'));
asserts.checkerror('filloutliers([1 2])', _('Not enough input arguments.'));
asserts.checkerror('filloutliers({1}, ''linear'')', sprintf(_('%s must be a real numeric array.'), 'A'));
asserts.checkerror('filloutliers([1 2], ''bad'')', sprintf(_('%s has an unsupported value.'), 'FILLMETHOD'));
asserts.checkerror('filloutliers([1 2], ''spline'')', sprintf(_('Unsupported %s.'), 'fill method'));
asserts.checkerror('filloutliers([1 2], ''linear'', ''bad'')', sprintf(_('%s has an unsupported value.'), 'METHOD'));
asserts.checkerror('filloutliers([1 2], ''linear'', ''OutlierLocations'', [true; false])', _('OutlierLocations must be a logical array the same size as A.'));
asserts.checkerror('filloutliers([1 2], ''linear'', ''SamplePoints'', [1 1])', _('SamplePoints must be sorted and contain unique values.'));
asserts.checkerror('filloutliers([1 2], ''linear'', ''DataVariables'', 1)', sprintf(_('%s is not supported for numeric array input.'), 'DataVariables'));
%=============================================================================
