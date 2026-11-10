%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
isNegZero = @(v) v == 0 & 1 ./ v < 0;
%=============================================================================
% erfcx of a large single value does not overflow
R = erfcx(single([10 30 90]));
assert_isequal(class(R), 'single');
assert_isapprox(R, single([0.0561409927438226 0.0187958888614168 0.00626838626086243]), 1e-6);
assert_isequal(erfcx(single([-Inf Inf NaN])), single([Inf 0 NaN]));
%=============================================================================
% erfinv and erf keep the sign of zero
assert_isequal(isNegZero(erfinv(-0)), true);
assert_isequal(isNegZero(erfinv(single(-0))), true);
assert_isequal(isNegZero(erf(-0)), true);
%=============================================================================
% single erfc in the subnormal range is rounded from the double value
assert_isequal(erfc(single(10)), single(erfc(10)));
assert_isequal(erfc(single(9.5)), single(erfc(9.5)));
%=============================================================================
