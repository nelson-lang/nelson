%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
ref = odeReferenceValues();
options = odeset('RelTol', 1e-6, 'AbsTol', 1e-9, 'MaxStep', 0.05);
[t45, y45] = ode45(@(t, y) y, [0 0.25 0.5], 1, options);
asserts.istrue(max(abs(t45 - ref.ode45Time)) < 1e-12);
asserts.istrue(max(abs(y45 - ref.ode45Exp)) < 5e-5);

[t23, y23] = ode23(@(t, y)[y(2); -y(1)], [0 pi / 4], [0; 1], options);
asserts.istrue(abs(t23(1) - ref.ode23Time(1)) < 1e-12);
asserts.istrue(abs(t23(length(t23)) - ref.ode23Time(2)) < 1e-12);
asserts.istrue(max(abs(y23(1, :) - ref.ode23Oscillator(1, :))) < 2e-4);
asserts.istrue(max(abs(y23(size(y23, 1), :) - ref.ode23Oscillator(2, :))) < 2e-4);

[tr, yr] = ode45(@(t, y) y, [0.5 0.25 0], exp(0.5), options);
asserts.istrue(max(abs(tr - ref.reverseTime)) < 1e-12);
asserts.istrue(max(abs(yr - ref.reverseExp)) < 5e-6);
%=============================================================================
eventOptions = odeset('Events', @odeReferenceEventHalf, 'InitialStep', 0.05, 'MaxStep', 0.05);
[t, y, te, ye, ie] = ode23(@(t, y) 1, [0 1], 0, eventOptions);
asserts.istrue(abs(t(length(t)) - 0.5) < 1e-10);
asserts.istrue(abs(y(length(y)) - 0.5) < 1e-10);
asserts.istrue(abs(te - 0.5) < 1e-8);
asserts.istrue(abs(ye - 0.5) < 1e-8);
asserts.isequal(ie, 1);

eventOptions = odeset('Events', @odeReferenceEventTwo, 'InitialStep', 0.05, 'MaxStep', 0.05);
[~, ~, te2, ye2, ie2] = ode45(@(t, y) 1, [0 1], 0, eventOptions);
asserts.istrue(max(abs(te2 - ref.eventTwoTime)) < 1e-8);
asserts.istrue(max(abs(ye2 - ref.eventTwoSolution)) < 1e-8);
asserts.isequal(ie2, ref.eventTwoIndex);
%=============================================================================
massOptions = odeset('Mass', 2, 'RelTol', 1e-4, 'AbsTol', 1e-7, 'MaxStep', 0.05);
[tm, ym] = ode15s(@(t, y) y, [0 0.5], 1, massOptions);
odeAssertFinalValue(tm, ym, 0.5, ref.massConstant, 1e-12, 2e-3);

massOptions = odeset('Mass', @(t, y) 2, 'RelTol', 1e-6, 'AbsTol', 1e-9, 'MaxStep', 0.05);
[tf, yf] = ode45(@(t, y) y, [0 0.5], 1, massOptions);
odeAssertFinalValue(tf, yf, 0.5, ref.massFunction, 1e-12, 5e-6);
%=============================================================================
implicitOptions = odeset('RelTol', 1e-6, 'AbsTol', 1e-8, 'MaxStep', 0.05);
[ti, yi] = ode15i(@(t, y, yp) yp + y, [0 0.5], 1, -1, implicitOptions);
odeAssertFinalValue(ti, yi, 0.5, ref.implicitHalf, 1e-12, 2e-3);
%=============================================================================
problem = ode('ODEFcn', @(t, y) - y, 'InitialValue', 1);
[f, data] = solutionFcn(problem, 0, 0.5);
asserts.istrue(abs(data.Time(length(data.Time)) - 0.5) < 1e-12);
asserts.istrue(abs(data.Solution(length(data.Solution)) - ref.objectSolutionHalf) < 5e-4);
asserts.istrue(abs(f(0.25) - ref.objectInterpolationQuarter) < 5e-4);
asserts.istrue(isa(data, 'nelson.ode.ODEResults'));
%=============================================================================
sol = ode45(@(t, y) y, [0 0.5], 1);
[v, vp] = deval(sol, ref.devalTimes);
asserts.istrue(max(abs(v - ref.devalExp)) < 5e-3);
asserts.istrue(max(abs(vp - ref.devalExp)) < 5e-3);

optionAliases = odeset('relt', 1e-4, 'abst', 1e-8);
asserts.isequal(odeget(optionAliases, 'REL'), 1e-4);
asserts.isequal(odeget(optionAliases, 'AbsT'), 1e-8);

eventOptions = odeset('Events', @odeReferenceEventHalf, 'InitialStep', 0.05, 'MaxStep', 0.05);
eventSol = ode45(@(t, y) 1, [0 1], 0, eventOptions);
extendedEventSol = odextend(eventSol, [], 1, odeset('MaxStep', 0.05));
asserts.istrue(abs(extendedEventSol.x(length(extendedEventSol.x)) - 1) < 1e-12);
asserts.istrue(abs(extendedEventSol.y(length(extendedEventSol.y)) - 1) < 1e-8);
asserts.istrue(abs(extendedEventSol.xe(1) - 0.5) < 1e-8);
asserts.istrue(abs(extendedEventSol.ye(1) - 0.5) < 1e-8);
asserts.isequal(extendedEventSol.ie(1), 1);
%=============================================================================
highOrderOptions = odeset('RelTol', 1e-6, 'AbsTol', 1e-9, 'MaxStep', 0.1);
requestedTimes = ref.highOrderTimes;
[t113, y113] = ode113(@(t, y) y, requestedTimes, 1, highOrderOptions);
[t78, y78] = ode78(@(t, y) y, requestedTimes, 1, highOrderOptions);
[t89, y89] = ode89(@(t, y) y, requestedTimes, 1, highOrderOptions);
sol113 = ode113(@(t, y) y, requestedTimes, 1, highOrderOptions);
sol78 = ode78(@(t, y) y, requestedTimes, 1, highOrderOptions);
sol89 = ode89(@(t, y) y, requestedTimes, 1, highOrderOptions);
asserts.istrue(max(abs(t113 - requestedTimes(:))) < 1e-12);
asserts.istrue(max(abs(t78 - requestedTimes(:))) < 1e-12);
asserts.istrue(max(abs(t89 - requestedTimes(:))) < 1e-12);
asserts.istrue(max(abs(y113 - ref.highOrderExp)) < 5e-5);
asserts.istrue(max(abs(y78 - ref.highOrderExp)) < 5e-5);
asserts.istrue(max(abs(y89 - ref.highOrderExp)) < 5e-5);
asserts.istrue(length(sol113.x) > length(requestedTimes));
asserts.istrue(length(sol78.x) > length(requestedTimes));
asserts.istrue(length(sol89.x) > length(requestedTimes));
asserts.istrue(abs(deval(sol78, requestedTimes(2)) - ref.highOrderExp(2)) < 5e-5);

massTimeOptions = odeset(highOrderOptions, 'Mass', @(t, y) 1 + t);
[tmt, ymt] = ode45(@(t, y) y, ref.massTime(:).', 1, massTimeOptions);
asserts.istrue(max(abs(tmt - ref.massTime)) < 1e-12);
asserts.istrue(max(abs(ymt - ref.massTimeSolution)) < 5e-5);

reverseEventOptions = odeset('Events', @odeReferenceEventReverseHalf, ...
  'InitialStep', 0.05, 'MaxStep', 0.05);
[trev, yrev, terev, yerev, ierev] = ode45(@(t, y) 1, [1 0], 1, reverseEventOptions);
asserts.istrue(abs(trev(length(trev)) - 0.5) < 1e-8);
asserts.istrue(abs(yrev(length(yrev)) - 0.5) < 1e-8);
asserts.istrue(abs(terev - 0.5) < 1e-8);
asserts.istrue(abs(yerev - 0.5) < 1e-8);
asserts.isequal(ierev, 1);

implicitParameterOptions = odeset('RelTol', 1e-6, 'AbsTol', 1e-8, ...
  'MaxStep', 0.05);
implicitParameterOptions.Parameters = 2;
[tip, yip] = ode15i(@(t, y, yp, rate) yp + rate * y, [0 0.5], 1, -2, implicitParameterOptions);
odeAssertFinalValue(tip, yip, 0.5, exp(-1), 1e-12, 2e-3);
%=============================================================================
