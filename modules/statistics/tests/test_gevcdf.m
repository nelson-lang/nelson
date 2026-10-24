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
asserts.isequal(nargin('gevcdf'), 5);
asserts.isequal(nargout('gevcdf'), 1);
%=============================================================================
x = [-2 -1 0 1 2];
R = gevcdf(x, 0.2, 1, 0);
REF = [2.599783710863074e-06 0.04727574940629059 0.3678794411714423 ...
       0.6690626526678188 0.8303280360778086];
asserts.isapprox(R, REF, 1e-15);
R = gevcdf(x, 0.2, 1, 0, 'upper');
REF = [0.9999974002162891 0.9527242505937095 0.6321205588285577 ...
       0.3309373473321812 0.1696719639221914];
asserts.isapprox(R, REF, 1e-15);
%=============================================================================
R = gevcdf([-6 -5 0 10], 0.2, 1, 0);
asserts.isapprox(R, [0 0 0.3678794411714423 0.9958932296031091], 1e-15);
R = gevcdf([0 5 6], -0.2, 1, 0);
asserts.isapprox(R, [0.3678794411714423 1 1], 1e-15);
%=============================================================================
R = gevcdf(single(x), single(0), single(1), single(0), 'u');
asserts.isequal(class(R), 'single');
asserts.isapprox(double(R), [0.99938202 0.93401194 0.63212055 0.30779937 0.12657697], 1e-6);
%=============================================================================
R = gevcdf([0 NaN 0 0], [0 0 NaN 0], [1 1 1 0], [0 0 0 NaN]);
asserts.isapprox(R(1), 0.3678794411714423, 1e-15);
asserts.istrue(all(isnan(R(2:4))));
R = gevcdf([], [], [], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
asserts.checkerror('gevcdf(1, 0, 1)', _('Not enough input arguments.'));
asserts.checkerror('gevcdf(1, 0, 1, 0, ''bad'')', sprintf(_('Invalid %s distribution option.'), 'generalized extreme value'));
asserts.checkerror('gevcdf(ones(2), ones(3), 1, 0)', _('Size mismatch on input arguments'));
asserts.checkerror('gevcdf({1}, 0, 1, 0)', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('gevcdf(1, {0}, 1, 0)', sprintf(_('%s must be real numeric.'), 'K'));
asserts.checkerror('gevcdf(1, 0, {1}, 0)', sprintf(_('%s must be real numeric.'), 'SIGMA'));
asserts.checkerror('gevcdf(1, 0, 1, {0})', sprintf(_('%s must be real numeric.'), 'MU'));
%=============================================================================
