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
asserts.isequal(nargin('nbincdf'), 4);
asserts.isequal(nargout('nbincdf'), 1);
%=============================================================================
x = 0:5;
R = nbincdf(x, 3, 0.4);
REF = [0.064 0.1792 0.31744 0.45568 0.580096 0.68460544];
asserts.isapprox(R, REF, 1e-15);
R = nbincdf(x, 3, 0.4, 'upper');
REF = [0.936 0.8208 0.68256 0.54432 0.419904 0.31539456];
asserts.isapprox(R, REF, 1e-15);
%=============================================================================
R = nbincdf([0.5 1.5 Inf], 3, 0.4);
asserts.isapprox(R, [0.064 0.1792 1], 1e-15);
R = nbincdf([-1 Inf], [3 3], [0.4 0], 'upper');
asserts.isequal(R, [1 1]);
%=============================================================================
R = nbincdf(single([0 1 2]), single(3), single(0.4));
asserts.isequal(class(R), 'single');
asserts.isapprox(double(R), [0.064 0.1792 0.31744], 1e-7);
%=============================================================================
R = nbincdf([0 NaN 0 0 0], [3 3 0 -1 NaN], 0.4);
asserts.isapprox(R(1), 0.064, 1e-15);
asserts.istrue(all(isnan(R(2:5))));
R = nbincdf([0 1], 3, [0 1]);
asserts.isequal(R, [0 1]);
%=============================================================================
R = nbincdf([], [], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
asserts.checkerror('nbincdf(ones(2), ones(3), 0.5)', _('Size mismatch on input arguments'));
asserts.checkerror('nbincdf(1, 3, 0.4, ''bad'')', sprintf(_('Invalid %s distribution option.'), 'negative binomial'));
asserts.checkerror('nbincdf({1}, 3, 0.4)', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('nbincdf(1, {3}, 0.4)', _('R must be a positive numeric array.'));
asserts.checkerror('nbincdf(1, 3, {0.4})', _('P must be a numeric array in the range [0,1].'));
%=============================================================================
