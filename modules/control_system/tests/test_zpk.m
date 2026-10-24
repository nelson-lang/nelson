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
% <--ENGLISH IMPOSED-->
%=============================================================================
assert_isequal(nargin('zpk'), -1);
assert_isequal(nargout('zpk'), 1);
%=============================================================================
sys = zpk();
assert_isequal(class(sys), 'zpk');
assert_istrue(isa(sys, 'zpk'));
assert_istrue(islti(sys));
%=============================================================================
p = properties(sys);
expectedProperties = {'Z'; 'P'; 'K'; 'Variable'; 'IODelay'; ...
  'InputDelay'; 'OutputDelay'; 'InputName'; 'InputUnit'; 'InputGroup'; ...
  'OutputName'; 'OutputUnit'; 'OutputGroup'; 'Notes'; 'UserData'; ...
  'Name'; 'Ts'; 'TimeUnit'; 'SamplingGrid'};
assert_isequal(p, expectedProperties);
assert_istrue(isprop(sys, 'Z'));
assert_istrue(isprop(sys, 'K'));
assert_isfalse(isprop(sys, 'A'));
%=============================================================================
% Build from (z, p, k)
G = zpk(-1, [-2 -3], 5);
assert_isequal(class(G), 'zpk');
assert_istrue(issiso(G));
assert_istrue(islti(G));
assert_istrue(isct(G));
assert_isfalse(isdt(G));
assert_isfalse(isstatic(G));
assert_isequal(G.Ts, 0);
assert_isequal(G.Variable, 's');
%=============================================================================
% Zero/pole/gain round-trip
assert_isequal(G.Z, {-1});
assert_isequal(G.P, {[-2; -3]});
assert_isequal(G.K, 5);
assert_isapprox(sort(pole(G)), [-3; -2], 1e-10);
assert_isapprox(sort(zero(G)), -1, 1e-10);
%=============================================================================
% Conversion to tf matches k*poly(z) / poly(p) (reference parity)
% Nelson stores the numerator padded to the denominator length, so
% 5(s+1)/((s+2)(s+3)) expands to [0 5 5] / [1 5 6].
T = tf(G);
assert_isequal(class(T), 'tf');
assert_isapprox(T.Numerator{1}, [0 5 5], 1e-12);
assert_isapprox(T.Denominator{1}, [1 5 6], 1e-12);
%=============================================================================
% zpk2tf helper returns the same transfer function
T2 = zpk2tf(G);
assert_isequal(class(T2), 'tf');
assert_isapprox(T2.Numerator{1}, [0 5 5], 1e-12);
assert_isapprox(T2.Denominator{1}, [1 5 6], 1e-12);
%=============================================================================
% Numerator / Denominator accessors (used by bode, rlocus, ...)
% must have matching lengths for the frequency-response loops.
assert_isapprox(G.Numerator{1}, [0 5 5], 1e-12);
assert_isapprox(G.Denominator{1}, [1 5 6], 1e-12);
assert_isequal(numel(G.Numerator{1}), numel(G.Denominator{1}));
%=============================================================================
% Conversion to ss and back through tf
S = ss(G);
assert_isequal(class(S), 'ss');
TS = tf(S);
assert_isapprox(sort(roots(TS.Denominator{1})), [-3; -2], 1e-10);
%=============================================================================
% Complex conjugate zeros expand to a real transfer function
G2 = zpk([-1+1i, -1-1i], -2, 1);
assert_isapprox(G2.Numerator{1}, [1 2 2], 1e-12);
assert_isapprox(G2.Denominator{1}, [0 1 2], 1e-12);
assert_istrue(isreal(G2.Numerator{1}));
%=============================================================================
% Factored display
R = evalc('display(G)');
assert_istrue(~isempty(strfind(R, '(s + 1)')));
assert_istrue(~isempty(strfind(R, '(s + 2)')));
assert_istrue(~isempty(strfind(R, '(s + 3)')));
assert_istrue(~isempty(strfind(R, 'Continuous-time zero-pole-gain model.')));
R2 = evalc('display(G2)');
assert_istrue(~isempty(strfind(R2, '(s^2 + 2s + 2)')));
%=============================================================================
% Static gain
Gs = zpk([], [], 7);
assert_istrue(isstatic(Gs));
assert_isequal(Gs.K, 7);
Rs = evalc('display(Gs)');
assert_istrue(~isempty(strfind(Rs, 'Static gain.')));
%=============================================================================
% Discrete-time model
Gd = zpk(0.5, [0.1 0.2], 1, 0.1);
assert_isequal(Gd.Ts, 0.1);
assert_isequal(Gd.Variable, 'z');
assert_istrue(isdt(Gd));
assert_isfalse(isct(Gd));
%=============================================================================
% Variable constructor
sv = zpk('s');
assert_isequal(sv.Variable, 's');
assert_isequal(sv.Ts, 0);
%=============================================================================
% Property name/value pairs
Gn = zpk(-1, [-2 -3], 5, 'InputName', {'u'}, 'Name', 'plant');
assert_isequal(Gn.InputName, {'u'});
assert_isequal(Gn.Name, 'plant');
%=============================================================================
% tf -> zpk conversion round-trip
Gback = zpk(tf([5 5], [1 5 6]));
assert_isapprox(sort(Gback.P{1}), [-3; -2], 1e-10);
assert_isapprox(Gback.Z{1}, -1, 1e-10);
assert_isapprox(Gback.K, 5, 1e-10);
%=============================================================================
% Negation
Gneg = -G;
assert_isequal(Gneg.K, -5);
%=============================================================================
% Arithmetic (returns tf)
Gp = G + zpk(-4, -5, 2);
assert_istrue(islti(Gp));
Gm = zpk(-1, [-2 -3], 1) * zpk(-4, [-5 -6], 1);
assert_istrue(islti(Gm));
%=============================================================================
