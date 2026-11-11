%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Two outputs, one input
sys = tf(ss(1, 1, [1; 2], [0; 0]));
asserts.isequal(size(sys), [2 1]);
asserts.isequal(sys.Numerator, {[0 1]; [0 2]});
asserts.isequal(sys.Denominator, {[1 -1]; [1 -1]});
%=============================================================================
% One output, two inputs
sys = tf(ss(-1, [1 2], 3, [0 1]));
asserts.isequal(size(sys), [1 2]);
asserts.isequal(sys.Numerator, {[0 3], [1 7]});
asserts.isequal(sys.Denominator, {[1 1], [1 1]});
%=============================================================================
% Two outputs, two inputs, each channel keeps only its own states
sys = tf(ss([-1 0; 0 -2], [1 0; 0 1], [1 1; 0 1], [0 0; 1 0]));
asserts.isequal(size(sys), [2 2]);
asserts.isequal(sys.Numerator, {[0 1], [0 1]; 1, [0 1]});
asserts.isequal(sys.Denominator, {[1 1], [1 2]; 1, [1 2]});
%=============================================================================
% Uncontrollable state is dropped
sys = tf(ss(diag([-1 -2]), [1; 0], [1 1], 0));
asserts.isequal(sys.Numerator, {[0 1]});
asserts.isequal(sys.Denominator, {[1 1]});
%=============================================================================
% Unobservable mode without zero pattern is kept
sys = tf(ss([-1 1; 0 -2], [1; 1], eye(2), 0));
asserts.isequal(sys.Numerator, {[0 1 3]; [0 1]});
asserts.isequal(sys.Denominator, {[1 3 2]; [1 2]});
%=============================================================================
% Discrete time with three outputs
sys = tf(ss([0 1; -2 -3], [0; 1], [1 0; 0 1; 1 1], [0; 0; 0], 0.1));
asserts.isequal(size(sys), [3 1]);
asserts.isequal(sys.Ts, 0.1);
asserts.isequal(sys.Variable, 'z');
% Third channel goes through null(): reference itself is a few ulps off
asserts.isequal(sys.Numerator(1:2), {[0 0 1]; [0 1 0]});
asserts.isapprox(sys.Numerator{3}, [0 1 1], 1e-14);
asserts.isequal(sys.Denominator, {[1 3 2]; [1 3 2]; [1 3 2]});
%=============================================================================
% Metadata follows the model
sysIn = ss(-1, 1, [1; 2], [0; 0]);
sysIn.OutputName = {'y1'; 'y2'};
sysIn.InputName = {'u'};
sys = tf(sysIn);
asserts.isequal(sys.OutputName, {'y1'; 'y2'});
asserts.isequal(sys.InputName, {'u'});
%=============================================================================
% Round trip keeps the frequency response
sysIn = ss([-1 1; 0 -2], [1 0; 1 1], [1 0; 0 1; 1 1], zeros(3, 2));
sys = tf(sysIn);
asserts.isapprox(evalfr(sys, 1i), evalfr(sysIn, 1i), 1e-12);
%=============================================================================
% Two by two numerator and denominator cells
sys = tf({1, 2; 3, 4}, {[1 1], [1 2]; [1 3], [1 4]});
asserts.isequal(sys.Numerator, {[0 1], [0 2]; [0 3], [0 4]});
asserts.isequal(sys.Denominator, {[1 1], [1 2]; [1 3], [1 4]});
%=============================================================================
% Channels reduced to gains give a static model
sys = tf(ss([-1 0; 0 -2], [1; 1], [0 0], 3));
asserts.istrue(isstatic(sys));
asserts.isequal(sys.Numerator, {3});
asserts.isequal(sys.Denominator, {1});
%=============================================================================
sys = tf(ss([-1 0; 0 -2], [1; 1], [0 0], 3, 0.5));
asserts.istrue(isstatic(sys));
asserts.isequal(sys.Ts, 0.5);
asserts.isequal(sys.Variable, 'z');
%=============================================================================
sys = tf(ss([-1 0; 0 -2], [0; 0], [1 1], 0));
asserts.istrue(isstatic(sys));
asserts.isequal(sys.Numerator, {0});
%=============================================================================
sys = tf(ss([-1 0; 0 -2], [1 0; 0 0], [0 0; 1 0], [3 0; 0 1]));
asserts.isfalse(isstatic(sys));
asserts.isequal(sys.Numerator, {3, 0; [0 1], 1});
asserts.isequal(sys.Denominator, {1, 1; [1 1], 1});
%=============================================================================
% Relative degree four keeps an exact numerator
sys = tf(ss([-2 1 0 0; 0 -3 1 0; 0 0 -4 1; 1 0 0 -5], [0; 0; 0; 1], [1 0 0 0], 0));
asserts.isequal(sys.Numerator, {[0 0 0 0 1]});
asserts.isapprox(sys.Denominator{1}, [1 14 71 154 119], 1e-12);
%=============================================================================
