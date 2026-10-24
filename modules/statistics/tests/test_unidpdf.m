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
asserts.isequal(nargin('unidpdf'), 2);
asserts.isequal(nargout('unidpdf'), 1);
%=============================================================================
x = 0:6;
R = unidpdf(x, 5);
asserts.isequal(R, [0 0.2 0.2 0.2 0.2 0.2 0]);
%=============================================================================
R = unidpdf([1 2.5 3 Inf NaN], 4);
asserts.isequal(R(1:4), [0.25 0 0.25 0]);
asserts.istrue(isnan(R(5)));
%=============================================================================
R = unidpdf([1 2 3], [1 2 3]);
asserts.isequal(R, [1 0.5 1 / 3]);
%=============================================================================
R = unidpdf(single([1 2 3]), single(4));
asserts.isequal(class(R), 'single');
asserts.isequal(R, single([0.25 0.25 0.25]));
%=============================================================================
R = unidpdf([1 1 1], [0 -1 1.5]);
asserts.istrue(all(isnan(R)));
R = unidpdf([], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
asserts.checkerror('unidpdf(1)', _('Not enough input arguments.'));
asserts.checkerror('unidpdf(1, 2, 3)', _('Too many input arguments.'));
asserts.checkerror('unidpdf(ones(2), ones(3))', _('Size mismatch on input arguments'));
asserts.checkerror('unidpdf({1}, 4)', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('unidpdf(1, {4})', _('N must be a positive integer numeric array.'));
%=============================================================================
