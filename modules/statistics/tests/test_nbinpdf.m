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
asserts.isequal(nargin('nbinpdf'), 3);
asserts.isequal(nargout('nbinpdf'), 1);
%=============================================================================
x = 0:5;
R = nbinpdf(x, 3, 0.4);
REF = [0.064 0.1152 0.13824 0.13824 0.124416 0.10450944];
asserts.isapprox(R, REF, 1e-15);
%=============================================================================
R = nbinpdf([0 1 2], [1 2.5 3], [0.5 0.4 1]);
REF = [0.5 0.151789327688082 0];
asserts.isapprox(R, REF, 1e-15);
%=============================================================================
R = nbinpdf(single([0 1 2]), single(3), single(0.4));
asserts.isequal(class(R), 'single');
asserts.isapprox(double(R), [0.064 0.1152 0.13824], 1e-7);
%=============================================================================
R = nbinpdf([-1 1.5 Inf NaN 0 0 0 0], [3 3 3 3 0 -1 NaN 3], 0.4);
asserts.isequal(R(1:3), [0 0 0]);
asserts.istrue(all(isnan(R(4:7))));
asserts.isapprox(R(8), 0.064, 1e-15);
R = nbinpdf([0 1], 3, [0 1]);
asserts.isequal(R, [0 0]);
R = nbinpdf([0 1], 3, [1 1]);
asserts.isequal(R, [1 0]);
%=============================================================================
R = nbinpdf([], [], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
asserts.checkerror('nbinpdf(ones(2), ones(3), 0.5)', _('Size mismatch on input arguments'));
asserts.checkerror('nbinpdf({1}, 3, 0.4)', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('nbinpdf(1, {3}, 0.4)', _('R must be a positive numeric array.'));
asserts.checkerror('nbinpdf(1, 3, {0.4})', _('P must be a numeric array in the range [0,1].'));
%=============================================================================
