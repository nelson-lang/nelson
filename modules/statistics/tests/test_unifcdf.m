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
asserts.isequal(nargin('unifcdf'), 4);
asserts.isequal(nargout('unifcdf'), 1);
%=============================================================================
x = [0 0.25 0.5 0.75 1];
R = unifcdf(x);
asserts.isequal(R, x);
R = unifcdf(x, 'upper');
asserts.isequal(R, 1 - x);
%=============================================================================
asserts.isequal(unifcdf(0.75), 0.75);
asserts.isequal(unifcdf(0.5, -1, 1), 0.75);
asserts.isequal(unifcdf(0.75, 'upper'), 0.25);
%=============================================================================
x = [-2 -1 0 1 2 3];
R = unifcdf(x, -1, 3);
asserts.isequal(R, [0 0 0.25 0.5 0.75 1]);
R = unifcdf(x, -1, 3, 'upper');
asserts.isequal(R, [1 1 0.75 0.5 0.25 0]);
%=============================================================================
R = unifcdf(single([0 0.5 1]), single(0), single(2));
asserts.isequal(class(R), 'single');
asserts.isequal(R, single([0 0.25 0.5]));
%=============================================================================
R = unifcdf([0 NaN 1 1], [0 0 2 1], [1 1 1 NaN]);
asserts.isequal(R(1), 0);
asserts.istrue(all(isnan(R(2:4))));
%=============================================================================
R = unifcdf([], [], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
asserts.checkerror('unifcdf(ones(2), ones(3), 1)', _('Size mismatch on input arguments'));
asserts.checkerror('unifcdf(1, 0)', _('Not enough input arguments.'));
asserts.checkerror('unifcdf(1, 0, 1, ''bad'')', sprintf(_('Invalid %s distribution option.'), 'uniform'));
asserts.checkerror('unifcdf({1}, 0, 1)', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('unifcdf(1, {0}, 1)', sprintf(_('%s must be real numeric.'), 'A'));
asserts.checkerror('unifcdf(1, 0, {1})', sprintf(_('%s must be real numeric.'), 'B'));
%=============================================================================
asserts.isapprox(unifcdf(0.5, 0, 2), 0.25, 1e-12);
%=============================================================================
