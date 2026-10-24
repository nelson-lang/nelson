%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('gammainc'), 3);
asserts.isequal(nargout('gammainc'), 1);
%=============================================================================
% reference values of the regularized lower incomplete gamma function
asserts.isapprox(gammainc(0.5, 2), 0.090204010431050, 1e-12);
asserts.isapprox(gammainc(5, 2), 0.959572318005487, 1e-12);
asserts.isapprox(gammainc(0.1, 0.5), 0.345279153981423, 1e-12);
%=============================================================================
% lower + upper == 1
asserts.isapprox(gammainc(3, 2) + gammainc(3, 2, 'upper'), 1, 1e-14);
asserts.isapprox(gammainc(0.5, 2, 'upper'), 1 - 0.090204010431050, 1e-12);
%=============================================================================
% boundary cases
asserts.isequal(gammainc(0, 2), 0);
asserts.isequal(gammainc(3, 0), 1);
asserts.isequal(gammainc(0, 0), 0);
%=============================================================================
% array + scalar expansion
R = gammainc([0.5 5; 0.1 10], 2);
REF = [0.090204010431050 0.959572318005487; 0.004678840160444 0.999500600772613];
asserts.isapprox(R, REF, 1e-10);
%=============================================================================
% single precision keeps single class
asserts.isequal(class(gammainc(single(0.5), single(2))), 'single');
%=============================================================================
asserts.checkerror('gammainc(-1, 2)', _('X and A must contain nonnegative values.'));
asserts.checkerror('gammainc(1, 2, ''middle'')', sprintf(_('%s expected.'), 'Wrong value of the third argument ''upper'' or ''lower'''));
%=============================================================================
