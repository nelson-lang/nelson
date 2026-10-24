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
asserts.isequal(nargin('normcdf'), 6);
asserts.isequal(nargout('normcdf'), 3);
%=============================================================================
x = [-2, -1, 0, 1, 2];
R = normcdf(x);
REF = [0.0227501319481792, 0.158655253931457, 0.5, ...
       0.841344746068543, 0.977249868051821];
asserts.isapprox(R, REF, 1e-12);
%=============================================================================
R = normcdf(x, 2, 3);
REF = [0.0912112197258679, 0.158655253931457, 0.252492537546923, ...
       0.369441340181764, 0.5];
asserts.isapprox(R, REF, 1e-12);
%=============================================================================
R = normcdf(x, 0, 1, 'upper');
REF = [0.977249868051821, 0.841344746068543, 0.5, ...
       0.158655253931457, 0.0227501319481792];
asserts.isapprox(R, REF, 1e-12);
%=============================================================================
R = normcdf(single(x));
asserts.isequal(class(R), 'single');
asserts.isapprox(R, single([0.0227501319481792, 0.158655253931457, 0.5, ...
                           0.841344746068543, 0.977249868051821]), 1e-6);
%=============================================================================
R = normcdf([], [], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
R = normcdf(0, [], []);
asserts.isequal(R, 0.5);
%=============================================================================
R = normcdf(0, 0, 1, 'u');
asserts.isequal(R, 0.5);
%=============================================================================
R = normcdf([0 1], [0 1], [1 2]);
asserts.isapprox(R, [0.5 0.5], 1e-12);
%=============================================================================
R = normcdf([0 Inf -Inf NaN], 0, 1);
asserts.isequal(R(1), 0.5);
asserts.isequal(R(2), 1);
asserts.isequal(R(3), 0);
asserts.istrue(isnan(R(4)));
%=============================================================================
R = normcdf(0, 0, 0);
asserts.istrue(isnan(R));
%=============================================================================
[p, plo, pup] = normcdf(0, 0, 1, [0.04 0; 0 0.01], 0.05);
asserts.isapprox(p, 0.5, 1e-12);
asserts.isapprox(plo, 0.347531768005586, 1e-12);
asserts.isapprox(pup, 0.652468231994414, 1e-12);
%=============================================================================
x = single([-1 0; 1 2]);
mu = single([0 0; 1 1]);
sigma = single([1 2; 1 0.5]);
pCov = single([0.04 0.01; 0.01 0.09]);
[p, plo, pup] = normcdf(x, mu, sigma, pCov, single(0.1));
asserts.isequal(class(p), 'single');
asserts.isequal(class(plo), 'single');
asserts.isequal(class(pup), 'single');
asserts.isequal(size(p), [2 2]);
asserts.isapprox(p, single([0.1586553 0.5; 0.5 0.9772499]), 1e-6);
asserts.isapprox(plo, single([0.0611083 0.4346745; 0.3710889 0.4277345]), 1e-6);
asserts.isapprox(pup, single([0.3247475 0.5653254; 0.6289111 0.9999856]), 1e-6);
%=============================================================================
[p, plo, pup] = normcdf(x, single(0), sigma, pCov, single(0.1));
asserts.isequal(class(p), 'single');
asserts.isequal(size(p), [2 2]);
asserts.isapprox(p, single([0.1586553 0.5; 0.8413448 0.9999683]), 1e-6);
asserts.isapprox(plo, single([0.0611083 0.4346745; 0.6416792 0.4566635]), 1e-6);
asserts.isapprox(pup, single([0.3247475 0.5653254; 0.9491899 1]), 1e-6);
%=============================================================================
asserts.checkerror('normcdf(ones(2), ones(3))', _('Size mismatch on input arguments'));
asserts.checkerror('normcdf(0, 0, 1, ''bad'')', sprintf(_('Invalid %s distribution option.'), 'normal'));
asserts.checkerror('normcdf(0, 0, 1, [1 0 0])', _('PCOV must be a 2-by-2 numeric matrix.'));
asserts.checkerror('normcdf(0, 0, 1, [1 0; 0 1], 2)', sprintf(_('%s must be a scalar in the range (0,1).'), 'ALPHA'));
%=============================================================================
asserts.istrue(isnan(normcdf(0, 0, -1)));
asserts.checkerror('normcdf({1})', sprintf(_('%s must be real numeric.'), 'X'));
%=============================================================================
asserts.isapprox(normcdf(0), 0.5, 1e-12);
%=============================================================================
