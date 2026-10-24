%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
sysA = tf(3);
sysB = tf(4);
sys = sysA + sysB;
asserts.isequal(sys.Numerator, {7});
asserts.isequal(sys.Denominator, {1});
asserts.isequal(sys.Ts, 0);
%=============================================================================
num = [3 4];
den = [3 1 5];
Ts = 0.2;
sys = tf(num, den, Ts);
R = sys + sys;
asserts.isequal(R.Numerator, {[0 18 30 38 40]});
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
sysStatic = tf(5);
sysDiscrete = tf(num, den, Ts1);
R = sysStatic + sysDiscrete;
asserts.isequal(R.Ts, Ts1);
R = sysDiscrete + tf('z');
asserts.isequal(R.Ts, Ts1);
%=============================================================================
sysA = tf(1);
sysB = tf(2);
asserts.isequal((sysA + sysB).UserData, []);
sysA.UserData = 'left';
asserts.isequal((sysA + sysB).UserData, 'left');
sysA.UserData = [];
sysB.UserData = 'right';
asserts.isequal((sysA + sysB).UserData, 'right');
sysA.UserData = 'left';
asserts.isequal((sysA + sysB).UserData, 'right');
%=============================================================================
sysA = tf([1 0], [1 1], 0.2);
sysB = tf([2 0], [1 2], 0.2);
sysA.Variable = 'z^-1';
asserts.isequal((sysA + sysB).Variable, 'z^-1');
sysA.Variable = 'z';
sysB.Variable = 'z';
asserts.isequal((sysA + sysB).Variable, 'z');
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
sys = sysA + sysB;
asserts.isequal(sys.InputName, {'u'});
asserts.isequal(sys.OutputName, {'y'});
asserts.isequal(sys.InputUnit, {'volt'});
asserts.isequal(sys.OutputUnit, {'meter'});
asserts.isequal(sys.TimeUnit, 'milliseconds');
sysB.InputName = {'v'};
sys = sysA + sysB;
asserts.isequal(sys.InputName, {});
%=============================================================================
