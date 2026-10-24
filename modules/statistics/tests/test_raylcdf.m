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
asserts.isequal(nargin('raylcdf'), 3);
asserts.isequal(nargout('raylcdf'), 1);
%=============================================================================
x = [-1 0 2 4 Inf];
R = raylcdf(x, 2);
E = [0 0 0.3934693402873666 0.8646647167633873 1];
asserts.isapprox(R, E, 1e-15);
R = raylcdf(x, 2, 'upper');
E = [1 1 0.6065306597126334 0.1353352832366127 0];
asserts.isapprox(R, E, 1e-15);
%=============================================================================
R = raylcdf([1 2 3], [1 2 3]);
E = [0.3934693402873666 0.3934693402873666 0.3934693402873666];
asserts.isapprox(R, E, 1e-15);
%=============================================================================
R = raylcdf(single([0 2 4]), single(2));
asserts.isequal(class(R), 'single');
asserts.isapprox(double(R), [0 0.3934693402873666 0.8646647167633873], 1e-7);
%=============================================================================
R = raylcdf([1 NaN 1 1], [1 1 NaN 0]);
asserts.isapprox(R(1), 0.3934693402873666, 1e-15);
asserts.istrue(all(isnan(R(2:4))));
%=============================================================================
R = raylcdf([], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
asserts.checkerror('raylcdf(1)', _('Not enough input arguments.'));
asserts.checkerror('raylcdf(ones(2), ones(3))', _('Size mismatch on input arguments'));
asserts.checkerror('raylcdf(1, 1, ''bad'')', sprintf(_('Invalid %s distribution option.'), 'Rayleigh'));
asserts.checkerror('raylcdf({1}, 1)', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('raylcdf(1, {1})', sprintf(_('%s must be real numeric.'), 'B'));
%=============================================================================
