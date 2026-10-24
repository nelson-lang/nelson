%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
options = odeset('relt', 1e-4, 'abst', 1e-8, 'maxs', 0.25);
asserts.isequal(odeget(options, 'RelTol'), 1e-4);
asserts.isequal(odeget(options, 'AbsTol'), 1e-8);
asserts.isequal(odeget(options, 'MaxStep'), 0.25);
asserts.isequal(odeget([], 'RelTol', 7), 7);

asserts.checkerror('odeset(''M'', 1)', sprintf(_('Ambiguous %s option name.'), 'ODE'));
asserts.checkerror('odeset(''NoSuchOption'', 1)', sprintf(_('Unknown %s option name.'), 'ODE'));
asserts.checkerror('ode45(@(t,y) y, [0 1], 1, odeset(''RelTol'', 0))', sprintf(_('%s must be positive.'), 'RelTol'));
asserts.checkerror('ode45(@(t,y) y, [0 1], 1, odeset(''AbsTol'', -1))', _('Positive absolute tolerance expected.'));
asserts.checkerror('ode45(@(t,y) y, [0 1], 1, odeset(''InitialStep'', 0))', sprintf(_('%s must be positive.'), 'InitialStep'));
asserts.checkerror('ode45(@(t,y) y, [0 1], 1, odeset(''MaxStep'', 0))', sprintf(_('%s must be positive.'), 'MaxStep'));
asserts.checkerror('ode45(@(t,y) y, [0 1], 1, odeset(''MinStep'', 0))', sprintf(_('%s must be positive.'), 'MinStep'));
asserts.checkerror('ode45(@(t,y) y, [0 1 0.5], 1)', _('Time span entries must strictly increase or decrease.'));
asserts.checkerror('ode45(@(t,y) y, [0 1; 2 3], 1)', _('Time span entries must strictly increase or decrease.'));
asserts.checkerror('ode45(@(t,y) y, [0 Inf], 1)', _('Time span entries must strictly increase or decrease.'));
asserts.checkerror('ode45(@(t,y) y, [0 1], [1; 2], odeset(''OutputSel'', 3))', sprintf(_('%s indices are invalid.'), 'OutputSel'));
asserts.checkerror('ode45(@(t,y) y, [0 1], [1; 2], odeset(''NonNegative'', 0))', sprintf(_('%s indices are invalid.'), 'NonNegative'));
asserts.checkerror('ode45(@(t,y) y, [0 1], 1, odeset(''Stats'', ''maybe''))', sprintf(_('%s must be ''on'' or ''off''.'), 'Stats'));
asserts.checkerror('ode15s(@(t,y) y, [0 1], 1, odeset(''BDF'', ''maybe''))', sprintf(_('%s must be ''on'' or ''off''.'), 'BDF'));
asserts.checkerror('ode15s(@(t,y) y, [0 1], 1, odeset(''JConstant'', ''maybe''))', sprintf(_('%s must be ''on'' or ''off''.'), 'JConstant'));
asserts.checkerror('ode15s(@(t,y) y, [0 1], 1, odeset(''MaxOrder'', 6))', sprintf(_('MaxOrder must be an integer scalar between %d and %d.'), 1, 5));
asserts.checkerror('ode15s(@(t,y) y, [0 1], 1, odeset(''LinearSolver'', ''bad''))', sprintf(_('%s has an invalid value.'), 'LinearSolver'));
asserts.checkerror('ode15s(@(t,y) y, [0 1], 1, odeset(''Preconditioner'', ''bad''))', sprintf(_('%s has an invalid value.'), 'Preconditioner'));
asserts.checkerror('ode15s(@(t,y) y, [0 1], 1, odeset(''MassSingular'', ''bad''))', sprintf(_('%s has an invalid value.'), 'MassSingular'));
asserts.checkerror('ode15s(@(t,y) y, [0 1], 1, odeset(''MStateDependence'', ''bad''))', sprintf(_('%s has an invalid value.'), 'MStateDependence'));
asserts.checkerror('ode45(@(t,y) y, [0 1], [1; 2], odeset(''Mass'', ones(3)))', sprintf(_('%s matrix has incompatible size.'), 'Mass'));
asserts.checkerror('ode45(@(t,y) y, [0 1], [1; 2], odeset(''Jacobian'', ones(3)))', sprintf(_('%s matrix has incompatible size.'), 'Jacobian'));
asserts.checkerror('ode15s(@(t,y) y, [0 1], [1; 2], odeset(''JPattern'', ones(3)))', sprintf(_('%s matrix has incompatible size.'), 'JPattern'));
asserts.checkerror('ode15s(@(t,y) y, [0 1], [1; 2], odeset(''JPattern'', {1}))', sprintf(_('%s must be a numeric or logical matrix.'), 'JPattern'));
asserts.checkerror('ode15s(@(t,y) y, [0 1], [1; 2], odeset(''MvPattern'', ones(3)))', sprintf(_('%s matrix has incompatible size.'), 'MvPattern'));
asserts.checkerror('ode15s(@(t,y) y, [0 1], [1; 2], odeset(''MvPattern'', {1}))', sprintf(_('%s must be a numeric or logical matrix.'), 'MvPattern'));
asserts.checkerror('odeMassMatrix({1})', sprintf(_('%s must be numeric or a function handle.'), 'Mass matrix'));
asserts.checkerror('odeMassMatrix([], ''MassSingular'', ''bad'')', sprintf(_('%s has an invalid value.'), 'MassSingular'));
asserts.checkerror('odeMassMatrix([], ''MStateDependence'', ''bad'')', sprintf(_('%s has an invalid value.'), 'MStateDependence'));
asserts.checkerror('odeMassMatrix([], ''MvPattern'', {1})', sprintf(_('%s must be numeric or logical.'), 'Mass vector pattern'));
asserts.checkerror('odeJacobian({1})', sprintf(_('%s must be numeric or a function handle.'), 'Jacobian'));
asserts.checkerror('odeJacobian([], ''Pattern'', {1})', sprintf(_('%s must be numeric or logical.'), 'Jacobian pattern'));
asserts.checkerror('odeJacobian([], ''Constant'', ''maybe'')', _('Jacobian constant flag must be ''on'' or ''off''.'));
%=============================================================================
