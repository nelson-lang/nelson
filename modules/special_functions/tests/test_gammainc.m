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
asserts.checkerror('gammainc(1, 2, ''middle'')', sprintf(_('%s expected.'), ...
  'Wrong value of the third argument ''upper'' or ''lower'''));
%=============================================================================
% infinite limits
assert_isequal(gammainc([Inf 1], [2 Inf]), [1 0]);
assert_isequal(gammainc(Inf, 2, 'upper'), 0);
assert_isequal(gammainc(single(Inf), single(3)), single(1));
%=============================================================================
% tiny upper tails are not rounded to zero
assert_isapprox(gammainc(700, 5, 'upper'), 9.9203914798000074e-295, 1e-12);
assert_isapprox(gammainc(690, 5, 'upper'), 2.0630697080063586e-290, 1e-12);
assert_isapprox(gammainc(700, 0.5, 'upper'), 2.1010145162642849e-306, 1e-12);
assert_isapprox(gammainc(720, 1, 'upper'), 2.0322308024183599e-313, 1e-10);
%=============================================================================
% large a near x = a
assert_isapprox(gammainc(1e5, 1e5), 0.50042052211036514, 1e-12);
assert_isapprox(gammainc(1e6, 1e6), 0.50013298076087254, 1e-12);
assert_isapprox(gammainc(0.99e6, 1e6), 5.4466446930105589e-24, 1e-9);
assert_isapprox(gammainc(1.003e6, 1e6), 0.99863825935378192, 1e-12);
%=============================================================================
