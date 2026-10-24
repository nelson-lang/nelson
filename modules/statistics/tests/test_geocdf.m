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
asserts.isequal(nargin('geocdf'), 3);
asserts.isequal(nargout('geocdf'), 1);
%=============================================================================
x = [0 1 2 5];
R = geocdf(x, 0.25);
asserts.isapprox(R, [0.25 0.4375 0.578125 0.822021484375], 1e-15);
R = geocdf(x, 0.25, 'upper');
asserts.isapprox(R, [0.75 0.5625 0.421875 0.177978515625], 1e-15);
%=============================================================================
R = geocdf([0.5 1.5 Inf], 0.25);
asserts.isapprox(R, [0.25 0.4375 1], 1e-15);
R = geocdf([-1 Inf], [0.25 0]);
asserts.isapprox(R, [0 0], 1e-15);
R = geocdf([-1 Inf], [0.25 0], 'upper');
asserts.isapprox(R, [1 1], 1e-15);
%=============================================================================
R = geocdf(single([0 1 2]), single(0.25));
asserts.isequal(class(R), 'single');
asserts.isapprox(double(R), [0.25 0.4375 0.578125], 1e-7);
%=============================================================================
R = geocdf([0 NaN 0 0], [0.25 0.25 -0.1 1.1]);
asserts.isapprox(R(1), 0.25, 1e-15);
asserts.istrue(all(isnan(R(2:4))));
%=============================================================================
R = geocdf([], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
asserts.checkerror('geocdf(ones(2), ones(3))', _('Size mismatch on input arguments'));
asserts.checkerror('geocdf(1, 0.5, ''bad'')', sprintf(_('Invalid %s distribution option.'), 'geometric'));
asserts.checkerror('geocdf({1}, 0.5)', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('geocdf(1, {0.5})', _('P must be a numeric array in the range [0,1].'));
%=============================================================================
asserts.isapprox(geocdf(2, 0.25), 1 - 0.75 ^ 3, 1e-12);
%=============================================================================
