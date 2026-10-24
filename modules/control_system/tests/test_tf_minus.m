%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
sysA = tf(3);
sysB = tf(4);
sys = sysA - sysB;
asserts.isequal(sys.Numerator, {-1});
asserts.isequal(sys.Denominator, {1});
asserts.isequal(sys.Ts, 0);
%=============================================================================
num = [3 4];
den = [3 1 5];
Ts = 0.2;
sys = tf(num, den, Ts);
R = sys - sys;
asserts.isequal(R.Numerator, {[0 0 0 0 0]});
asserts.isequal(R.Denominator, {[9 6 31 10 25]});
asserts.isequal(R.Ts, 0.2);
%=============================================================================
num = [3 4];
den = [3 1 5];
Ts1 = 0.2;
sys1 = tf(num, den, Ts1);
Ts2 = 0.4;
sys2 = tf(num, den, Ts2);
asserts.checkerror('R = sys1 + sys2;', _('Sampling times must agree.'));
%=============================================================================
sysA = tf([1 0], [1 1], 0.2);
sysB = tf([2 0], [1 2], 0.2);
sysB.Variable = 'z^-1';
asserts.isequal((sysA - sysB).Variable, 'z^-1');
%=============================================================================
sysA = tf(1);
sysB = tf(2);
sysA.InputName = {'u'};
sysB.InputName = {'u'};
sysA.OutputName = {'y'};
sysB.OutputName = {'y'};
sysA.InputUnit = {'volt'};
sysB.InputUnit = {'volt'};
sysA.OutputUnit = {'meter'};
sysB.OutputUnit = {'meter'};
sysA.TimeUnit = 'milliseconds';
sysB.TimeUnit = 'milliseconds';
sys = sysA - sysB;
asserts.isequal(sys.InputName, {'u'});
asserts.isequal(sys.OutputName, {'y'});
asserts.isequal(sys.InputUnit, {'volt'});
asserts.isequal(sys.OutputUnit, {'meter'});
asserts.isequal(sys.TimeUnit, 'milliseconds');
%=============================================================================
