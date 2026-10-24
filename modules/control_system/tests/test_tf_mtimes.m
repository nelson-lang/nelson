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
sys = sysA * sysB;
REF = tf(12);
asserts.isequal(sys, REF);
%=============================================================================
num = [3 4];
den = [3 1 5];
Ts = 0.2;
sys = tf(num, den, Ts);
R = sys * 2;
REF = tf([6 8], [3 1 5], Ts);
asserts.isequal(R, REF);
%=============================================================================
R = tf(2) * sys;
asserts.isequal(R.Ts, Ts);
R = tf('z') * sys;
asserts.isequal(R.Ts, Ts);
%=============================================================================
sysA = tf(1);
sysB = tf(2);
sysA.UserData = 'left';
sysB.UserData = 'right';
asserts.isequal((sysA * sysB).UserData, 'right');
%=============================================================================
sysA = tf([1 0], [1 1], 0.2);
sysB = tf([2 0], [1 2], 0.2);
sysA.Variable = 'z^-1';
asserts.isequal((sysA * sysB).Variable, 'z^-1');
%=============================================================================
sysA = tf(1);
sysB = tf(2);
sysA.OutputName = {'plant output'};
sysA.OutputUnit = {'meter'};
sysA.TimeUnit = 'milliseconds';
sysB.InputName = {'controller input'};
sysB.InputUnit = {'volt'};
sysB.TimeUnit = 'milliseconds';
sys = sysA * sysB;
asserts.isequal(sys.OutputName, {'plant output'});
asserts.isequal(sys.OutputUnit, {'meter'});
asserts.isequal(sys.InputName, {'controller input'});
asserts.isequal(sys.InputUnit, {'volt'});
asserts.isequal(sys.TimeUnit, 'milliseconds');
%=============================================================================
