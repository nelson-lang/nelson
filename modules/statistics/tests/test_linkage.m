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
asserts.isequal(nargin('linkage'), 4);
asserts.isequal(nargout('linkage'), 1);
%=============================================================================
X = [0 0; 1 0; 0 2; 4 4];
Z = linkage(X);
REF = [1 2 1; 3 5 2; 4 6 4.47213595499958];
asserts.isapprox(Z, REF, 1e-12);
%=============================================================================
D = squareform([0 1 2 3; 1 0 4 5; 2 4 0 6; 3 5 6 0]);
Z = linkage(D, 'complete');
asserts.isequal(Z, [1 2 1; 3 5 4; 4 6 6]);
%=============================================================================
Z = linkage(X, 'average');
REF = [1 2 1; 3 5 2.118033988749895; 4 6 5.042996734830653];
asserts.isapprox(Z, REF, 1e-12);
%=============================================================================
Z = linkage(X, 'weighted');
REF = [1 2 1; 3 5 2.118033988749895; 4 6 4.900281539872885];
asserts.isapprox(Z, REF, 1e-12);
%=============================================================================
Z = linkage(X, 'centroid');
REF = [1 2 1; 3 5 2.061552812808830; 4 6 4.955356249106169];
asserts.isapprox(Z, REF, 1e-12);
%=============================================================================
Z = linkage(X, 'median');
REF = [1 2 1; 3 5 2.061552812808830; 4 6 4.802343178074637];
asserts.isapprox(Z, REF, 1e-12);
%=============================================================================
Z = linkage(X, 'ward');
REF = [1 2 1; 3 5 2.380476142847617; 4 6 6.069047152011042];
asserts.isapprox(Z, REF, 1e-12);
%=============================================================================
Z = linkage(X, 'single', 'cityblock');
asserts.isequal(Z, [1 2 1; 3 5 2; 4 6 6]);
%=============================================================================
Z = linkage(X, 'single', 'minkowski', 1.5);
REF = [1 2 1; 3 5 2; 4 6 4.894521629542950];
asserts.isapprox(Z, REF, 1e-12);
%=============================================================================
asserts.checkerror('linkage()', _('Not enough input arguments.'));
asserts.checkerror('linkage({1})', _('Input must be a real numeric matrix or vector.'));
asserts.checkerror('linkage([1 2])', _('Distance vector length must be a binomial coefficient.'));
asserts.checkerror('linkage([1 2], ''single'')', _('Distance vector length must be a binomial coefficient.'));
asserts.checkerror('linkage([1; 2], ''unknown'')', _('Invalid linkage method.'));
asserts.checkerror('linkage(ones(1, 2))', _('Distance vector length must be a binomial coefficient.'));
asserts.checkerror('linkage(ones(1, 2, 2))', _('Input must be a real numeric matrix or vector.'));
%=============================================================================
