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
asserts.isequal(nargin('gevpdf'), 4);
asserts.isequal(nargout('gevpdf'), 1);
%=============================================================================
x = [-2 -1 0 1 2];
R = gevpdf(x, 0.2, 1, 0);
REF = [5.572238749277855e-05 0.1803426719905494 0.3678794411714423 ...
       0.2240677286508631 0.1102761227355588];
asserts.isapprox(R, REF, 1e-15);
%=============================================================================
R = gevpdf([-6 -5 0 10], 0.2, 1, 0);
REF = [0 0 0.3678794411714423 0.001366108682583140];
asserts.isapprox(R, REF, 1e-15);
R = gevpdf([0 5 6], -0.2, 1, 0);
asserts.isapprox(R, [0.3678794411714423 0 0], 1e-15);
%=============================================================================
R = gevpdf(single(x), single(0), single(1), single(0));
asserts.isequal(class(R), 'single');
asserts.isapprox(double(R), [0.0045662816 0.17937408 0.36787945 0.25464639 0.11820495], 1e-6);
%=============================================================================
R = gevpdf([0 NaN 0 0], [0 0 NaN 0], [1 1 1 0], [0 0 0 NaN]);
asserts.isapprox(R(1), 0.3678794411714423, 1e-15);
asserts.istrue(all(isnan(R(2:4))));
R = gevpdf([], [], [], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
asserts.checkerror('gevpdf(1, 0, 1)', _('Not enough input arguments.'));
asserts.checkerror('gevpdf(ones(2), ones(3), 1, 0)', _('Size mismatch on input arguments'));
asserts.checkerror('gevpdf({1}, 0, 1, 0)', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('gevpdf(1, {0}, 1, 0)', sprintf(_('%s must be real numeric.'), 'K'));
asserts.checkerror('gevpdf(1, 0, {1}, 0)', sprintf(_('%s must be real numeric.'), 'SIGMA'));
asserts.checkerror('gevpdf(1, 0, 1, {0})', sprintf(_('%s must be real numeric.'), 'MU'));
%=============================================================================
