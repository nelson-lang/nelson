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
A = [1 2; 3 4];
B = [1 0; 0 1];
C = [1 1; 1 1];
D = [0 0; 0 0];
sys = ss(A, B, C, D);
R = sys + sys
%=============================================================================
A = [1 2; 3 4];
B = [1 0; 0 1];
C = [1 1; 1 1];
D = [0 0; 0 0];
sys1 = ss(A, B, C, D, 0.2);
sys2 = ss(A, B, C, D, 0.4);
asserts.checkerror('R = sys1 + sys2;', _('Sampling times must agree.'));
%=============================================================================
sysStatic = ss(5);
R = sysStatic + sys1;
asserts.isequal(R.Ts, 0.2);
%=============================================================================
sysA = ss(1);
sysB = ss(2);
asserts.isequal((sysA + sysB).UserData, []);
sysA.UserData = 'left';
asserts.isequal((sysA + sysB).UserData, 'left');
sysA.UserData = [];
sysB.UserData = 'right';
asserts.isequal((sysA + sysB).UserData, 'right');
sysA.UserData = 'left';
asserts.isequal((sysA + sysB).UserData, 'right');
%=============================================================================
sysA = ss(1);
sysB = ss(2);
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
sysB.OutputName = {'z'};
sys = sysA + sysB;
asserts.isequal(sys.OutputName, {});
%=============================================================================
