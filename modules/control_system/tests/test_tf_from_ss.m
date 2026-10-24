%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
Fs = 5;
dt = 1 / Fs;
A = [cos(dt) sin(dt); -sin(dt) cos(dt)];
B = [1 - cos(dt); sin(dt)];
C = [-1 0];
D = 1;
sysIn = ss(A, B, C, D);
sysIn.InputName = {'u'};
sysIn.OutputName = {'y'};
sysIn.InputUnit = {'volt'};
sysIn.OutputUnit = {'meter'};
sysIn.TimeUnit = 'milliseconds';
sysIn.Name = 'state model';
sysIn.UserData = 321;

sys = tf(sysIn);
[N, D] = tfdata(sys);

N_REF = [1.0000 -1.9801 0.9801];
D_REF = [1.0000 -1.9601 1.0000];

asserts.isapprox(N, N_REF, 1e-4);
asserts.isapprox(D, D_REF, 1e-4);
asserts.isequal(sys.InputName, {'u'});
asserts.isequal(sys.OutputName, {'y'});
asserts.isequal(sys.InputUnit, {'volt'});
asserts.isequal(sys.OutputUnit, {'meter'});
asserts.isequal(sys.TimeUnit, 'milliseconds');
asserts.isequal(sys.Name, 'state model');
asserts.isequal(sys.UserData, 321);
%=============================================================================
A = [-1 0 0; 0 -4 -2.5; 0 2 0];
B = [2 0; 0 2; 0 0];
C = [-1 0.5 0.5];
D = [1 0];
sysIn = ss(A, B, C, D);

sys = tf(sysIn);
[N, D] = tfdata(sys);

N_REF = {[1 -1], [1.0000 5.0000 7.0000]};
D_REF = {[1 1], [1.0000 4.0000 5.0000]};

asserts.isapprox(N{1}, N_REF{1}, 1e-4);
asserts.isapprox(D{1}, D_REF{1}, 1e-4);
asserts.isapprox(N{2}, N_REF{2}, 1e-4);
asserts.isapprox(D{2}, D_REF{2}, 1e-4);

%=============================================================================
sysIn = ss(5);
sysIn.InputName = {'u'};
sysIn.OutputName = {'y'};
sysIn.TimeUnit = 'milliseconds';
sysIn.UserData = 654;
sys = tf(sysIn);
asserts.isequal(sys.Numerator, {5});
asserts.isequal(sys.InputName, {'u'});
asserts.isequal(sys.OutputName, {'y'});
asserts.isequal(sys.TimeUnit, 'milliseconds');
asserts.isequal(sys.UserData, 654);
%=============================================================================
