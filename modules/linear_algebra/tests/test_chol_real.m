%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('chol'), 1);
assert_isequal(nargout('chol'), 1);
%=============================================================================
R = chol([]);
REF = [];
assert_isequal(R, REF);
%=============================================================================
assert_checkerror('chol([1 2;3 4])', _('Positive finite matrix expected.'));
%=============================================================================
A = [      1.4834      1.5929      0.6517      1.7214      1.3592;
1.5929      2.4007      0.8535      2.5614      2.0422;
0.6517      0.8535      0.7954      1.0757      0.6566;
1.7214      2.5614      1.0757      2.8708      2.1766;
1.3592      2.0422      0.6566      2.1766      1.7726];
%=============================================================================
B = A * A';
assert_isapprox(B, B', 1e-9);
R = chol(B);
REF = [      3.1580      4.4104      1.7697      4.8242      3.7534;
0.0000      0.5558      0.1027      0.5958      0.4848;
0.0000      0.0000      0.4813      0.1711     -0.0744;
0.0000      0.0000      0.0000      0.0585      0.0127;
0.0000      0.0000      0.0000      0.0000      0.0158];
assert_isapprox(R, REF, 1e-3);
%=============================================================================
B_reconstructed = R' * R;
assert_isapprox(B, B_reconstructed, 1e-9);
assert_istrue(istriu(R));
%=============================================================================
