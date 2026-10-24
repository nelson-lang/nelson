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
asserts.isequal(nargin('rmoutliers'), -2);
asserts.isequal(nargout('rmoutliers'), 6);
%=============================================================================
A = [57 59 60 100 59 58 57 58 300 61 62 60 62 58 57];
[B, TFrm, TFoutlier] = rmoutliers(A);
asserts.isequal(B, [57 59 60 59 58 57 58 61 62 60 62 58 57]);
asserts.isequal(TFrm, logical([0 0 0 1 0 0 0 0 1 0 0 0 0 0 0]));
asserts.isequal(TFoutlier, TFrm);
B = rmoutliers(A, 'mean');
asserts.isequal(B, [57 59 60 100 59 58 57 58 61 62 60 62 58 57]);
%=============================================================================
M = [2 290 1 2; 1 0 323 1; 0 2 3 2; 1 1 2 3];
[B, TFrm, TFoutlier] = rmoutliers(M);
asserts.isequal(B, [0 2 3 2; 1 1 2 3]);
asserts.isequal(TFrm, logical([1; 1; 0; 0]));
asserts.isequal(TFoutlier, logical([0 1 0 0; 0 0 1 0; 0 0 0 0; 0 0 0 0]));
%=============================================================================
N = [1 1 1; 1 100 1; 1 100 1; 1 1 100; 1 1 1];
[B, TFrm] = rmoutliers(N, 2);
asserts.isequal(B, [1; 1; 1; 1; 1]);
asserts.isequal(TFrm, logical([0 1 1]));
%=============================================================================
detect = isoutlier(A);
[B, TFrm, TFoutlier, L, U, C] = rmoutliers(A, 'OutlierLocations', detect);
asserts.isequal(B, [57 59 60 59 58 57 58 61 62 60 62 58 57]);
asserts.isequal(TFrm, detect);
asserts.isequal(TFoutlier, detect);
asserts.istrue(isnan(L));
asserts.istrue(isnan(U));
asserts.istrue(isnan(C));
%=============================================================================
P = [1 1 100 1 1];
B = rmoutliers(P, 'movmedian', 3, 'SamplePoints', [1 2 100 101 102]);
asserts.isequal(B, P);
B = rmoutliers(P, 'movmedian', 3);
asserts.isequal(B, [1 1 1 1]);
%=============================================================================
Q = single([1; 1; 10; 1]);
B = rmoutliers(Q);
asserts.isequal(class(B), 'single');
asserts.isequal(B, single([1; 1; 1]));
%=============================================================================
asserts.checkerror('rmoutliers()', _('Not enough input arguments.'));
asserts.checkerror('rmoutliers({1})', sprintf(_('%s must be a real numeric vector or matrix.'), 'A'));
asserts.checkerror('rmoutliers(ones(2,2,2))', sprintf(_('%s must be a real numeric vector or matrix.'), 'A'));
asserts.checkerror('rmoutliers([1 2], 3)', _('Name-value arguments must be paired.'));
asserts.checkerror('rmoutliers([1 2], ''bad'')', sprintf(_('%s has an unsupported value.'), 'METHOD'));
asserts.checkerror('rmoutliers([1 2], ''OutlierLocations'', [true; false])', _('OutlierLocations must be a logical array the same size as A.'));
asserts.checkerror('rmoutliers([1 2], ''movmean'', 2, ''SamplePoints'', [1 1])', _('SamplePoints must be sorted and contain unique values.'));
%=============================================================================
