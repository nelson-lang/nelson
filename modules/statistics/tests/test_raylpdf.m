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
asserts.isequal(nargin('raylpdf'), 2);
asserts.isequal(nargout('raylpdf'), 1);
%=============================================================================
x = [-1 0 2 4 Inf];
R = raylpdf(x, 2);
E = [0 0 0.3032653298563167 0.1353352832366127 0];
asserts.isapprox(R, E, 1e-15);
%=============================================================================
R = raylpdf([1 2 3], [1 2 3]);
E = [0.6065306597126334 0.3032653298563167 0.2021768865708778];
asserts.isapprox(R, E, 1e-15);
%=============================================================================
R = raylpdf(single([0 2 4]), single(2));
asserts.isequal(class(R), 'single');
asserts.isapprox(double(R), [0 0.3032653298563167 0.1353352832366127], 1e-7);
%=============================================================================
R = raylpdf([1 NaN 1 1], [1 1 NaN 0]);
asserts.isapprox(R(1), 0.6065306597126334, 1e-15);
asserts.istrue(all(isnan(R(2:4))));
%=============================================================================
R = raylpdf([], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
asserts.checkerror('raylpdf(1)', _('Not enough input arguments.'));
asserts.checkerror('raylpdf(ones(2), ones(3))', _('Size mismatch on input arguments'));
asserts.checkerror('raylpdf({1}, 1)', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('raylpdf(1, {1})', sprintf(_('%s must be real numeric.'), 'B'));
%=============================================================================
