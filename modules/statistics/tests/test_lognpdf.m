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
asserts.isequal(nargin('lognpdf'), 3);
asserts.isequal(nargout('lognpdf'), 1);
%=============================================================================
x = [0 1 exp(1)];
R = lognpdf(x);
E = [0 0.3989422804014327 0.0890160549159515];
asserts.isapprox(R, E, 1e-15);
%=============================================================================
R = lognpdf(exp([0 1 2]), 1, 2);
E = [0.17603266338214976 0.19947114020071635 0.17603266338214976] ./ exp([0 1 2]);
asserts.isapprox(R, E, 1e-15);
%=============================================================================
R = lognpdf(single([1 exp(1)]), single(0), single(1));
asserts.isequal(class(R), 'single');
asserts.isapprox(double(R), [0.3989422804014327 0.0890160549159515], 1e-7);
%=============================================================================
R = lognpdf([1 NaN 1 1], [0 0 NaN 0], [1 1 1 0]);
asserts.isapprox(R(1), 0.3989422804014327, 1e-15);
asserts.istrue(all(isnan(R(2:4))));
%=============================================================================
R = lognpdf([], [], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
asserts.checkerror('lognpdf(ones(2), ones(3), 1)', _('Size mismatch on input arguments'));
asserts.checkerror('lognpdf({1}, 0, 1)', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('lognpdf(1, {0}, 1)', sprintf(_('%s must be real numeric.'), 'MU'));
asserts.checkerror('lognpdf(1, 0, {1})', sprintf(_('%s must be real numeric.'), 'SIGMA'));
%=============================================================================
