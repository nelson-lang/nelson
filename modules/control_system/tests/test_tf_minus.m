%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
sysA = tf(3);
sysB = tf(4);
sys = sysA - sysB;
assert_isequal(sys.Numerator, {-1});
assert_isequal(sys.Denominator, {1});
assert_isequal(sys.Ts, -2);
%=============================================================================
num = [3 4];
den = [3 1 5];
Ts = 0.2;
sys = tf(num, den, Ts);
R = sys - sys; 
assert_isequal(R.Numerator, {[0 0 0 0 0]});
assert_isequal(R.Denominator,  {[9 6 31 10 25]});
assert_isequal(R.Ts, 0.2);
%=============================================================================
num = [3 4];
den = [3 1 5];
Ts1 = 0.2;
sys1 = tf(num, den, Ts1);
Ts2 = 0.4;
sys2 = tf(num, den, Ts2);
assert_checkerror('R = sys1 + sys2;', _('Sampling times must agree.'));
%=============================================================================
