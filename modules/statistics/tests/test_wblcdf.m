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
asserts.isequal(nargin('wblcdf'), 4);
asserts.isequal(nargout('wblcdf'), 1);
%=============================================================================
asserts.isapprox(wblcdf(3), 0.950212931632136, 1e-15);
R = wblcdf([0 2 4 Inf], 2, 3);
E = [0 0.6321205588285577 0.9996645373720975 1];
asserts.isapprox(R, E, 1e-15);
R = wblcdf([0 2 4 Inf], 2, 3, 'upper');
E = [1 0.36787944117144233 0.00033546262790251185 0];
asserts.isapprox(R, E, 1e-15);
%=============================================================================
R = wblcdf(0.2:0.2:1, 1:5);
E = 1 - exp(-(0.2:0.2:1) ./ (1:5));
asserts.isapprox(R, E, 1e-15);
%=============================================================================
R = wblcdf(single([0 2 4]), single(2), single(3));
asserts.isequal(class(R), 'single');
asserts.isapprox(double(R), [0 0.6321205588285577 0.9996645373720975], 1e-7);
%=============================================================================
R = wblcdf([1 NaN 1 1], [1 1 NaN 0], [1 1 1 1]);
asserts.isapprox(R(1), 0.6321205588285577, 1e-15);
asserts.istrue(all(isnan(R(2:4))));
%=============================================================================
R = wblcdf([], [], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
asserts.checkerror('wblcdf(ones(2), ones(3), 1)', _('Size mismatch on input arguments'));
asserts.checkerror('wblcdf(1, 1, 1, ''bad'')', sprintf(_('Invalid %s distribution option.'), 'Weibull'));
asserts.checkerror('wblcdf({1}, 1, 1)', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('wblcdf(1, {1}, 1)', sprintf(_('%s must be real numeric.'), 'A'));
asserts.checkerror('wblcdf(1, 1, {1})', sprintf(_('%s must be real numeric.'), 'B'));
%=============================================================================
