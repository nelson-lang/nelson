%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
sysA = tf(1);
sysB = tf(2);
sysA.UserData = 'left';
sysB.UserData = 'right';
asserts.isequal((sysA / sysB).UserData, 'right');
%=============================================================================
sysA = tf([1 0], [1 1], 0.2);
sysB = tf([2 0], [1 2], 0.2);
sysB.Variable = 'z^-1';
asserts.isequal((sysA / sysB).Variable, 'z^-1');
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
sys = sysA / sysB;
asserts.isequal(sys.InputName, {'u'});
asserts.isequal(sys.OutputName, {'y'});
asserts.isequal(sys.InputUnit, {'volt'});
asserts.isequal(sys.OutputUnit, {'meter'});
asserts.isequal(sys.TimeUnit, 'milliseconds');
%=============================================================================
