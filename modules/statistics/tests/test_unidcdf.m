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
asserts.isequal(nargin('unidcdf'), 2);
asserts.isequal(nargout('unidcdf'), 1);
%=============================================================================
x = 0:6;
R = unidcdf(x, 5);
asserts.isequal(R, [0 0.2 0.4 0.6 0.8 1 1]);
%=============================================================================
R = unidcdf([0.5 1.5 2.5 4 Inf NaN], 4);
asserts.isequal(R(1:5), [0 0.25 0.5 1 1]);
asserts.istrue(isnan(R(6)));
%=============================================================================
R = unidcdf([1 2 3], [1 2 3]);
asserts.isequal(R, [1 1 1]);
%=============================================================================
R = unidcdf(single([1 2 3]), single(4));
asserts.isequal(class(R), 'single');
asserts.isequal(R, single([0.25 0.5 0.75]));
%=============================================================================
R = unidcdf([1 1 1], [0 -1 1.5]);
asserts.istrue(all(isnan(R)));
R = unidcdf([], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
asserts.checkerror('unidcdf(1)', _('Not enough input arguments.'));
asserts.checkerror('unidcdf(ones(2), ones(3))', _('Size mismatch on input arguments'));
asserts.checkerror('unidcdf({1}, 4)', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('unidcdf(1, {4})', _('N must be a positive integer numeric array.'));
%=============================================================================
