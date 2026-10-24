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
sys1 = tf(2, [1 3 0]);
sys2 = tf(1, [8 2 1]);
sys = append(sys1, 10, sys2);

num_REF = {[0 0 2], [0], [0]; [0], [10], [0]; [0], [0], [0 0 1]};
asserts.isequal(sys.Numerator, num_REF);

den_REF = {[1 3 0], [1], [1]; [1], [1], [1]; [1], [1], [8 2 1]};
asserts.isequal(sys.Denominator, den_REF);
%=============================================================================
sys1 = ss(0, 1, 1, 0);
sys2 = ss(5, 4, 3, 2);
sys = append(sys1, 10, sys2);

A_REF = [0 0; 0 5];
B_REF = [1 0 0; 0 0 4];
C_REF = [1 0; 0 0; 0 3];
D_REF = [0 0 0; 0 10 0; 0 0 2];
asserts.isequal(sys.A, A_REF);
asserts.isequal(sys.B, B_REF);
asserts.isequal(sys.C, C_REF);
asserts.isequal(sys.D, D_REF);
%=============================================================================
sys1 = tf(1, [1 0]);
sys2 = tf([1 -1], [4 2]);
sys = append(sys1, 10, sys2)

num_REF = {[0 1], [0], [0]; [0], [10], [0]; [0], [0], [1 -1]};
den_REF = {[1 0], [1], [1]; [1], [1], [1]; [1], [1], [4 2]};
asserts.isequal(sys.Numerator, num_REF);
asserts.isequal(sys.Denominator, den_REF);
%=============================================================================
sysd = tf(1, [1 -0.5], 0.2);
sys = append(tf(2), sysd);
asserts.isequal(sys.Ts, 0.2);
sys = append(ss(2), ss(0, 1, 1, 0, 0.2));
asserts.isequal(sys.Ts, 0.2);
%=============================================================================
sys1 = tf(1);
sys2 = tf(2);
sys1.UserData = 'left';
sys2.UserData = 'right';
sys = append(sys1, sys2);
asserts.isequal(sys.UserData, 'right');
sys1 = tf([1 0], [1 1], 0.2);
sys2 = tf([2 0], [1 2], 0.2);
sys2.Variable = 'z^-1';
sys = append(sys1, sys2);
asserts.isequal(sys.Variable, 'z^-1');
sys1 = tf(1);
sys2 = tf(2);
sys1.InputName = {'u1'};
sys1.OutputName = {'y1'};
sys1.InputUnit = {'volt'};
sys1.OutputUnit = {'meter'};
sys1.TimeUnit = 'milliseconds';
sys2.InputName = {'u2'};
sys2.OutputName = {'y2'};
sys2.InputUnit = {'ampere'};
sys2.OutputUnit = {'radian'};
sys2.TimeUnit = 'milliseconds';
sys = append(sys1, sys2);
asserts.isequal(sys.InputName, {'u1', 'u2'});
asserts.isequal(sys.OutputName, {'y1', 'y2'});
asserts.isequal(sys.InputUnit, {'volt', 'ampere'});
asserts.isequal(sys.OutputUnit, {'meter', 'radian'});
asserts.isequal(sys.TimeUnit, 'milliseconds');
sys1 = ss(1);
sys2 = ss(2);
sys1.UserData = 'left';
sys2.UserData = 'right';
sys = append(sys1, sys2);
asserts.isequal(sys.UserData, 'right');
sys1 = ss(1);
sys2 = ss(2);
sys1.InputName = {'u1'};
sys1.OutputName = {'y1'};
sys1.InputUnit = {'volt'};
sys1.OutputUnit = {'meter'};
sys1.TimeUnit = 'milliseconds';
sys2.InputName = {'u2'};
sys2.OutputName = {'y2'};
sys2.InputUnit = {'ampere'};
sys2.OutputUnit = {'radian'};
sys2.TimeUnit = 'milliseconds';
sys = append(sys1, sys2);
asserts.isequal(sys.InputName, {'u1', 'u2'});
asserts.isequal(sys.OutputName, {'y1', 'y2'});
asserts.isequal(sys.InputUnit, {'volt', 'ampere'});
asserts.isequal(sys.OutputUnit, {'meter', 'radian'});
asserts.isequal(sys.TimeUnit, 'milliseconds');
%=============================================================================
