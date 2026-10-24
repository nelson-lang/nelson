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
asserts.isequal(nargin('ss'), -1);
asserts.isequal(nargout('ss'), 1);
%=============================================================================
sys = ss();
asserts.isequal(class(sys), 'ss');
asserts.istrue(isa(sys, 'ss'));
p = properties(sys);
expectedProperties = {'A'; 'B'; 'C'; 'D'; 'E'; 'Offsets'; 'Scaled'; 'StateName'; ...
  'StatePath'; 'StateUnit'; 'InternalDelay'; 'InputDelay'; 'OutputDelay'; ...
  'InputName'; 'InputUnit'; 'InputGroup'; 'OutputName'; 'OutputUnit'; ...
  'OutputGroup'; 'Notes'; 'UserData'; 'Name'; 'Ts'; 'TimeUnit'; ...
  'SamplingGrid'};
asserts.isequal(p, expectedProperties);
asserts.istrue(isprop(sys, 'A'));
asserts.istrue(isprop(sys, 'SamplingGrid'));
asserts.isfalse(isprop(sys, 'Numerator'));
%=============================================================================
A = [-15, -20; 10, 0];
B = [5; 0];
C = [0, 10];
D = 0;
sys = ss(A, B, C, D);
asserts.isequal(class(sys), 'ss');
asserts.isequal(sys.A, A);
asserts.isequal(sys.B, B);
asserts.isequal(sys.C, C);
asserts.isequal(sys.D, D);
asserts.isequal(sys.Ts, 0);
asserts.isequal(sys.UserData, []);
%=============================================================================
A = [1 2; 3 4];
B = [1 2; 3 4];
C = [5 6; 7 8];
D = [9 10; 11 12];
sys = ss(A, B, C, D);
sys.InputName = {'u1', 'u2'};
sys.OutputName = {'y1', 'y2'};
sys.InputUnit = {'volt', 'ampere'};
sys.OutputUnit = {'meter', 'radian'};
sys.TimeUnit = 'milliseconds';
sys.UserData = 123;
subsys = sys(2, 1);
asserts.isequal(subsys.A, A);
asserts.isequal(subsys.B, [1; 3]);
asserts.isequal(subsys.C, [7 8]);
asserts.isequal(subsys.D, 11);
asserts.isequal(subsys.InputName, {'u1'});
asserts.isequal(subsys.OutputName, {'y2'});
asserts.isequal(subsys.InputUnit, {'volt'});
asserts.isequal(subsys.OutputUnit, {'radian'});
asserts.isequal(subsys.TimeUnit, 'milliseconds');
asserts.isequal(subsys.UserData, 123);
subsys = sys(:, 2);
asserts.isequal(subsys.B, [2; 4]);
asserts.isequal(subsys.C, C);
asserts.isequal(subsys.D, [10; 12]);
asserts.isequal(subsys.InputName, {'u2'});
asserts.isequal(subsys.OutputName, {'y1', 'y2'});
sys(1, 2) = ss(99);
asserts.isequal(sys.D, [9 99; 11 12]);
sys(2, 1).D = 88;
asserts.isequal(sys.D, [9 99; 88 12]);
replacement = ss(A, [20; 30], [40 50], 60);
sys(1, 1) = replacement;
asserts.isequal(sys.B(:, 1), [20; 30]);
asserts.isequal(sys.C(1, :), [40 50]);
asserts.isequal(sys.D(1, 1), 60);
asserts.checkerror('sys(1, 1) = ss(A + eye(2), [1; 2], [3 4], 5);', _('State-space assignment must preserve the state matrix.'));
asserts.checkerror('sys(1, 1) = ss(1, 1, 1, 1, 0.5);', _('Sampling times must agree.'));
asserts.checkerror('sys{1};', sprintf(_('Illegal indexing structure argument: type %s expected.'), "'.'"));
%=============================================================================
A = [1 2 3; 4 5 6; 7 8 9];
B = [10; 11; 12];
C = [];
D = [];
sys = ss(A, B, C, D);
asserts.isequal(sys.A, A);
asserts.isequal(sys.B, B);
asserts.isequal(sys.C, zeros(0, 3));
asserts.isequal(sys.D, zeros(0, 1));
%=============================================================================
asserts.checkerror('sys = ss([], 3, [], [])', sprintf(_('The number of rows in matrices %s and %s must be equal.'), 'A', 'B'));
%=============================================================================
A = [0 10; -20 -30];
B = [0 10];
C = [10 0];
D = 0;
asserts.checkerror('sys = ss(A, B, C, D)', sprintf(_('The number of rows in matrices %s and %s must be equal.'), 'A', 'B'));
%=============================================================================
A = [1 0.0000 4.9200 -4.9200 0.0000 0.0000 0.0000;
  2 -12.500 0.0000 0.0000 0.0000 0.0000 0.0000;
  0.0000 3.3300 -3.3300 0.0000 0.0000 0.0000 0.0000;
  3 0.0000 0.0000 0.0000 -0.5450 0.0000 0.0000;
  5.0000 0.0000 0.0000 4.9200 -0.04165 0.0000 4.9200;
  0.0000 6.0000 0.0000 0.0000 -5.2100 -12.500 0.0000;
  0.0000 0.0000 6.0000 0.0000 0.0000 3.3300 -3.3300];
B = [0.0000 0.0000;
  23.5000 0.0000;
  0.0000 0.0000;
  0.0000 0.0000;
  0.0000 0.0000;
  0.0000 23.500;
  0.0000 0.0000];
C = [1.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000;
  0.0000 0.0000 0.0000 1.0000 0.0000 0.0000 0.0000;
  0.0000 0.0000 0.0000 0.0000 1.0000 0.0000 0.0000];
D = [];
sys = ss(A, B, C, D);
asserts.isapprox(sys.A, A, 1e-3);
asserts.isapprox(sys.B, B, 1e-3);
asserts.isapprox(sys.C, C, 1e-3);
asserts.isequal(sys.D, zeros(3, 2));
%=============================================================================
D = [20, 40; 30, 50];
sys = ss(D);
asserts.isequal(sys.A, []);
asserts.isequal(sys.B, zeros(0, 2));
asserts.isequal(sys.C, zeros(2, 0));
asserts.isequal(sys.D, D);
asserts.isequal(sys.Ts, 0);
asserts.istrue(isstatic(sys));
%=============================================================================
sys = ss(5);
asserts.isequal(sys.Ts, 0);
R = evalc('display(sys)');
asserts.istrue(~isempty(strfind(R, ['     ', _('Static gain.')])));
%=============================================================================
A = -1;
B = 2;
C = 3;
D = 4;
sys = ss(A, B, C, D, 'StateName', {'x'}, 'Name', 'plant');
asserts.isequal(sys.StateName, {'x'});
asserts.isequal(sys.Name, 'plant');
sys.StateName{1} = 'state';
sys.StatePath = {'block/x'};
sys.StateUnit = {'meter'};
sys.Offsets = struct('x', 1, 'u', 2, 'y', 3, 'dx', 4);
sys.InternalDelay = [0.1 0.2];
sys.InputDelay = 1;
sys.OutputDelay = 2;
sys.InputName = {'u'};
sys.OutputName = {'y'};
sys.InputUnit = {'volt'};
sys.OutputUnit = {'meter'};
sys.InputGroup = struct('controls', 1);
sys.OutputGroup = struct('measurements', 1);
sys.Notes = ["state model"];
sys.UserData = 7;
sys.TimeUnit = 'milliseconds';
sys.SamplingGrid = struct('speed', 20);
st = struct(sys);
asserts.isequal(st.StateName, {'state'});
asserts.isequal(st.StatePath, {'block/x'});
asserts.isequal(st.StateUnit, {'meter'});
asserts.isequal(st.Offsets.x, 1);
asserts.isequal(st.InternalDelay, [0.1 0.2]);
asserts.isequal(st.InputDelay, 1);
asserts.isequal(st.OutputDelay, 2);
asserts.isequal(st.InputName, {'u'});
asserts.isequal(st.OutputName, {'y'});
asserts.isequal(st.InputGroup.controls, 1);
asserts.isequal(st.OutputGroup.measurements, 1);
asserts.isequal(st.UserData, 7);
asserts.isequal(st.TimeUnit, 'milliseconds');
asserts.isequal(st.SamplingGrid.speed, 20);
asserts.checkerror('ss(A, B, C, D, -2);', _('Ts property should be either a positive scalar, 0, or -1 to indicate that it is unspecified.'));
asserts.checkerror('sys.TimeUnit = ''fortnights'';', sprintf(_('%s must be a valid time unit.'), 'TimeUnit property'));
asserts.checkerror('sys.Ts = -2;', _('Ts property should be either a positive scalar, 0, or -1 to indicate that it is unspecified.'));
%=============================================================================
num = [3 4];
den = [3 1 5];
Ts = 0.2;
sysIn = tf(num, den, Ts)
sysIn.InputName = {'u'};
sysIn.OutputName = {'y'};
sysIn.InputUnit = {'volt'};
sysIn.OutputUnit = {'meter'};
sysIn.TimeUnit = 'milliseconds';
sysIn.Name = 'transfer model';
sysIn.UserData = 321;
sys = ss(sysIn);

A_REF = [-0.3333 -1.6667; 1.0000 0];
B_REF = [2; 0];
C_REF = [0.5000 0.6667];
D_REF = 0;
Ts_REF = Ts;

asserts.isapprox(sys.A, A_REF, 1e-4);
asserts.isapprox(sys.B, B_REF, 1e-4);
asserts.isapprox(sys.C, C_REF, 1e-4);
asserts.isapprox(sys.D, D_REF, 1e-4);
asserts.isapprox(Ts, Ts_REF, 1e-4);
asserts.isequal(sys.InputName, {'u'});
asserts.isequal(sys.OutputName, {'y'});
asserts.isequal(sys.InputUnit, {'volt'});
asserts.isequal(sys.OutputUnit, {'meter'});
asserts.isequal(sys.TimeUnit, 'milliseconds');
asserts.isequal(sys.Name, 'transfer model');
asserts.isequal(sys.UserData, 321);
%=============================================================================
sysIn = tf(5);
sysIn.InputName = {'u'};
sysIn.OutputName = {'y'};
sysIn.TimeUnit = 'milliseconds';
sysIn.UserData = 654;
sys = ss(sysIn);
asserts.isequal(sys.D, 5);
asserts.isequal(sys.InputName, {'u'});
asserts.isequal(sys.OutputName, {'y'});
asserts.isequal(sys.TimeUnit, 'milliseconds');
asserts.isequal(sys.UserData, 654);
%=============================================================================
