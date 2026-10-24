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
asserts.isequal(nargin('tf'), -1);
asserts.isequal(nargout('tf'), 1);
sys = tf();
asserts.isequal(class(sys), 'tf');
asserts.istrue(isa(sys, 'tf'));
p = properties(sys);
expectedProperties = {'Numerator'; 'Denominator'; 'Variable'; 'IODelay'; ...
  'InputDelay'; 'OutputDelay'; 'InputName'; 'InputUnit'; 'InputGroup'; ...
  'OutputName'; 'OutputUnit'; 'OutputGroup'; 'Notes'; 'UserData'; ...
  'Name'; 'Ts'; 'TimeUnit'; 'SamplingGrid'};
asserts.isequal(p, expectedProperties);
asserts.istrue(isprop(sys, 'Numerator'));
asserts.istrue(isprop(sys, 'SamplingGrid'));
asserts.isfalse(isprop(sys, 'A'));
%=============================================================================
sys = tf(5);
asserts.isequal(sys.Ts, 0);
asserts.istrue(isstatic(sys));
R = evalc('display(sys)');
asserts.istrue(~isempty(strfind(R, 'Static gain.')));
%=============================================================================
s = tf('s');
asserts.isequal(s.Variable, 's');
asserts.isequal(s.Ts, 0);
p = tf('p');
asserts.isequal(p.Variable, 'p');
z = tf('z');
asserts.isequal(z.Variable, 'z');
asserts.isequal(z.Ts, -1);
z = tf('z', 0.2);
asserts.isequal(z.Variable, 'z');
asserts.isequal(z.Ts, 0.2);
q = tf('q', -1);
asserts.isequal(q.Variable, 'q');
asserts.isequal(q.Ts, -1);
%=============================================================================
sys = tf([1 2], [1 3], 0.1, 'Variable', 'z^-1', 'TimeUnit', 'milliseconds', 'InputName', {'u'});
asserts.isequal(sys.Variable, 'z^-1');
asserts.isequal(sys.TimeUnit, 'milliseconds');
asserts.isequal(sys.InputName, {'u'});
sys.OutputName = {'y'};
sys.OutputName{1} = 'plant output';
sys.InputUnit = {'volt'};
sys.OutputUnit = {'meter'};
sys.IODelay = 2;
sys.InputDelay = 1;
sys.OutputDelay = 3;
sys.Name = 'plant';
sys.Notes = ["created by test"];
sys.UserData = 42;
sys.SamplingGrid = struct('speed', 10);
st = struct(sys);
asserts.isequal(st.OutputName, {'plant output'});
asserts.isequal(st.InputUnit, {'volt'});
asserts.isequal(st.OutputUnit, {'meter'});
asserts.isequal(st.IODelay, 2);
asserts.isequal(st.InputDelay, 1);
asserts.isequal(st.OutputDelay, 3);
asserts.isequal(st.Name, 'plant');
asserts.isequal(st.UserData, 42);
asserts.isequal(st.SamplingGrid.speed, 10);
asserts.checkerror('tf([1 2], [1 3], -2);', _('Ts property should be either a positive scalar, 0, or -1 to indicate that it is unspecified.'));
asserts.checkerror('sys.Variable = ''x'';', sprintf(_('%s expected.'), "'s', 'p', 'z', 'q', 'z^-1', or 'q^-1'"));
asserts.checkerror('sys.Variable = ''s'';', _('Continuous-time variable requires a continuous-time model.'));
asserts.checkerror('sys.TimeUnit = ''fortnights'';', sprintf(_('%s must be a valid time unit.'), 'TimeUnit property'));
asserts.checkerror('sys.Ts = -2;', _('Ts property should be either a positive scalar, 0, or -1 to indicate that it is unspecified.'));
%=============================================================================
sys = tf([1 2], [1 3]);
asserts.checkerror('sys.Variable = ''z'';', _('Discrete-time variable requires a discrete-time model.'));
sys.Ts = 0.25;
sys.Variable = 'q';
asserts.isequal(sys.Variable, 'q');
asserts.isequal(sys.Ts, 0.25);
%=============================================================================
numerator = [2, 0];
denominator = [4, 0, 3, -1];
sys = tf(numerator, denominator);
R = evalc('display(sys)');
REF =  ' 
sys =
 
        2 s
  ———————————————
  4 s^3 + 3 s - 1
 
Continuous-time transfer function.
 
';
asserts.isequal(R, REF);
%=============================================================================
numerator = [2, 0];
denominator = [4, 0, 3, -1];
ts = 1.5;
sys = tf(numerator, denominator, ts);
R = evalc('display(sys)');
REF =  ' 
sys =
 
        2 z
  ———————————————
  4 z^3 + 3 z - 1
 
Sample time: 1.5000 seconds
Discrete-time transfer function.
 
';
asserts.isequal(R, REF);
%=============================================================================
numerator = {[2, 0], [1, 3]};
denominator = {[4, 0, 3, -1], [1, 3, 5]};
sys = tf(numerator, denominator);
R = evalc('display(sys)');
REF =  ' 
sys =
 
  From input 1 to output:
        2 s
  ———————————————
  4 s^3 + 3 s - 1
 
  From input 2 to output:
      s + 3
  —————————————
  s^2 + 3 s + 5
 
Continuous-time transfer function.
 
';
asserts.isequal(R, REF);
Numerator_REF = {[0 0 2 0], [0 1 3]};
Denominator_REF = {[4 0 3 -1], [1 3 5]};
asserts.isequal(sys.Numerator, Numerator_REF);
asserts.isequal(sys.Denominator, Denominator_REF);
%=============================================================================
numerator = {1, 2; 3, 4};
denominator = {1, 1; 1, 1};
sys = tf(numerator, denominator);
sys.InputName = {'u1', 'u2'};
sys.OutputName = {'y1', 'y2'};
sys.InputUnit = {'volt', 'ampere'};
sys.OutputUnit = {'meter', 'radian'};
sys.TimeUnit = 'milliseconds';
sys.UserData = 123;
subsys = sys(2, 1);
asserts.isequal(size(subsys), [1 1]);
asserts.isequal(subsys.Numerator{1}, 3);
asserts.isequal(subsys.InputName, {'u1'});
asserts.isequal(subsys.OutputName, {'y2'});
asserts.isequal(subsys.InputUnit, {'volt'});
asserts.isequal(subsys.OutputUnit, {'radian'});
asserts.isequal(subsys.TimeUnit, 'milliseconds');
asserts.isequal(subsys.UserData, 123);
subsys = sys(:, 2);
asserts.isequal(size(subsys), [2 1]);
asserts.isequal(subsys.InputName, {'u2'});
asserts.isequal(subsys.OutputName, {'y1', 'y2'});
sys(1, 2) = tf(9);
asserts.isequal(sys.Numerator{1, 2}, 9);
asserts.isequal(sys.Denominator{1, 2}, 1);
sys(2, 2).Numerator = 12;
asserts.isequal(sys.Numerator{2, 2}, 12);
replacement = tf({8; 9}, {1; 1});
sys(:, 1) = replacement;
asserts.isequal(sys.Numerator{1, 1}, 8);
asserts.isequal(sys.Numerator{2, 1}, 9);
sysDynamic = tf({[1 0], [2 0]; [3 0], [4 0]}, {[1 1], [1 1]; [1 1], [1 1]});
asserts.checkerror('sysDynamic(1, 1) = tf([1 0], [1 1], 0.5);', _('Sampling times must agree.'));
asserts.checkerror('sys(1, 1) = tf({1, 2}, {1, 1});', _('Matrix dimensions must agree.'));
asserts.checkerror('sys{1};', sprintf(_('Illegal indexing structure argument: type %s expected.'), "'.' or '()'"));
%=============================================================================
numerator = {[2, 0], [1, 3], [4, 6]};
denominator = {[4, 0, 3, -1], [1, 3, 5]};
asserts.checkerror('sys = tf(numerator, denominator);', _('Numerator and Denominator must have compatible sizes.'));
%=============================================================================
