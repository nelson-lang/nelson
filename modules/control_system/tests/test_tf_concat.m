%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ENGLISH IMPOSED-->
%=============================================================================
sys1 = tf([1 -1], [1 1]);
sys2 = tf([1 2], [1 4 5]);
sys1.UserData = 'left';
sys2.UserData = 'right';
sys = [sys1; sys2];
asserts.isequal(sys.UserData, 'right');
R = evalc('display(sys)');
REF =  ' 
sys =
 
  1:
   s - 1
  ———————
   s + 1
 
  2:
      s + 2
  —————————————
  s^2 + 4 s + 5
 
Continuous-time transfer function.
 
';
asserts.isequal(R, REF)
%=============================================================================
sys1 = tf([1 -1], [1 1]);
sys2 = tf([1 2], [1 4 5]);
sys1.UserData = 'left';
sys2.UserData = 'right';
sys = [sys1, sys2];
asserts.isequal(sys.UserData, 'right');
R = evalc('display(sys)');
REF =  ' 
sys =
 
  From input 1 to output:
   s - 1
  ———————
   s + 1
 
  From input 2 to output:
      s + 2
  —————————————
  s^2 + 4 s + 5
 
Continuous-time transfer function.
 
';
asserts.isequal(R, REF)
%=============================================================================
sys1 = tf([1 0], [1 1], 0.2);
sys2 = tf([2 0], [1 2], 0.2);
sys2.Variable = 'z^-1';
sys = [sys1, sys2];
asserts.isequal(sys.Variable, 'z^-1');
sys = [sys1; sys2];
asserts.isequal(sys.Variable, 'z^-1');
%=============================================================================
sys1 = tf(1);
sys2 = tf(2);
sys1.InputName = {'u1'};
sys2.InputName = {'u2'};
sys1.InputUnit = {'volt'};
sys2.InputUnit = {'ampere'};
sys1.OutputName = {'y'};
sys2.OutputName = {'y'};
sys1.OutputUnit = {'meter'};
sys2.OutputUnit = {'meter'};
sys1.TimeUnit = 'milliseconds';
sys2.TimeUnit = 'milliseconds';
sys = [sys1, sys2];
asserts.isequal(sys.InputName, {'u1', 'u2'});
asserts.isequal(sys.InputUnit, {'volt', 'ampere'});
asserts.isequal(sys.OutputName, {'y'});
asserts.isequal(sys.OutputUnit, {'meter'});
asserts.isequal(sys.TimeUnit, 'milliseconds');
sys1 = tf(1);
sys2 = tf(2);
sys1.InputName = {'u'};
sys2.InputName = {'u'};
sys1.InputUnit = {'volt'};
sys2.InputUnit = {'volt'};
sys1.OutputName = {'y1'};
sys2.OutputName = {'y2'};
sys1.OutputUnit = {'meter'};
sys2.OutputUnit = {'radian'};
sys1.TimeUnit = 'milliseconds';
sys2.TimeUnit = 'seconds';
sys = [sys1; sys2];
asserts.isequal(sys.InputName, {'u'});
asserts.isequal(sys.InputUnit, {'volt'});
asserts.isequal(sys.OutputName, {'y1', 'y2'});
asserts.isequal(sys.OutputUnit, {'meter', 'radian'});
asserts.isequal(sys.TimeUnit, 'milliseconds');
%=============================================================================
