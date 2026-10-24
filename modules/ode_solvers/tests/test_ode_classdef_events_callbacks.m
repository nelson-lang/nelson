%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 16-->
paramProblem = ode('ODEFcn', @(t, y, rate, threshold) rate, ...
  'InitialValue', 0, ...
  'Parameters', {2, 0.5}, ...
  'EventDefinition', odeEvent(@odeTestParameterizedEvent));
paramResult = solve(paramProblem, 0, 1);
asserts.istrue(abs(paramResult.Time(length(paramResult.Time)) - 0.25) < 1e-8);
asserts.istrue(abs(paramResult.EventTime(1) - 0.25) < 1e-8);
asserts.istrue(abs(paramResult.EventSolution(1) - 0.5) < 1e-8);

wrappedParamProblem = ode('ODEFcn', @(t, y, rate, threshold) rate, ...
  'InitialValue', 0, ...
  'Parameters', {2, 0.5}, ...
  'EventDefinition', odeEvent(@(t, y, rate, threshold) odeTestParameterizedEvent(t, y, rate, threshold)));
wrappedParamResult = solve(wrappedParamProblem, 0, 0.5);
asserts.istrue(abs(wrappedParamResult.Time(length(wrappedParamResult.Time)) - 0.5) < 1e-8);
asserts.istrue(abs(wrappedParamResult.EventTime(1) - 0.25) < 1e-8);
asserts.istrue(abs(wrappedParamResult.EventSolution(1) - 0.5) < 1e-8);

legacyTwoEventProblem = ode('ODEFcn', @(t, y) 1, ...
  'InitialValue', 0, ...
  'EventDefinition', odeEvent(@odeTestTwoEvents));
legacyTwoEventResult = solve(legacyTwoEventProblem, 0, 0.6);
asserts.isequal(length(legacyTwoEventResult.EventTime), 2);
asserts.istrue(abs(legacyTwoEventResult.Time(length(legacyTwoEventResult.Time)) - 0.5) < 1e-8);
asserts.istrue(abs(legacyTwoEventResult.EventTime(1) - 0.25) < 1e-8);
asserts.istrue(abs(legacyTwoEventResult.EventTime(2) - 0.5) < 1e-8);
asserts.isequal(legacyTwoEventResult.EventIndex(:), [1; 2]);

objectTwoEvent = odeEvent('EventFcn', @(t, y)[y(1) - 0.25; y(1) - 0.5], ...
  'Response', {'proceed'; 'stop'}, ...
  'Direction', {'increasing'; 'increasing'});
objectTwoEventProblem = ode('ODEFcn', @(t, y) 1, ...
  'InitialValue', 0, ...
  'EventDefinition', objectTwoEvent);
objectTwoEventResult = solve(objectTwoEventProblem, 0, 0.6);
asserts.isequal(length(objectTwoEventResult.EventTime), 2);
asserts.istrue(abs(objectTwoEventResult.Time(length(objectTwoEventResult.Time)) - 0.5) < 1e-8);
asserts.isequal(objectTwoEventResult.EventIndex(:), [1; 2]);

badSizeEvent = odeEvent('EventFcn', @(t, y)[y(1) - 0.25; y(1) - 0.5], ...
  'Response', {'proceed'; 'stop'; 'stop'});
badSizeProblem = ode('ODEFcn', @(t, y) 1, ...
  'InitialValue', 0, ...
  'EventDefinition', badSizeEvent);
asserts.checkerror('solve(badSizeProblem, 0, 0.6);', _('Response size does not match event value size.'));
%=============================================================================
eventProperties = {'EventFcn'; 'Direction'; 'Response'; 'CallbackFcn'};
massProperties = {'MassMatrix'; 'Singular'; 'StateDependence'; 'SparsityPattern'};
jacobianProperties = {'Jacobian'; 'SparsityPattern'};
delayProperties = {'History'; 'ValueDelay'; 'SlopeDelay'};
sensitivityProperties = {'Method'; 'ParameterIndices'; 'Jacobian'; 'InitialValue'; 'InitialSlope'; ...
  'ObjectiveFcn'; 'QuadratureFcn'; 'ObjectiveTime'; 'AdjointRelativeTolerance'; ...
  'AdjointAbsoluteTolerance'};
resultProperties = {'Time'; 'Solution'; 'Sensitivity'; 'EventTime'; 'EventSolution'; 'EventIndex'; ...
  'EventSensitivity'; 'AdjointGradient'};
asserts.isequal(properties(odeEvent()), eventProperties);
asserts.isequal(properties(odeMassMatrix()), massProperties);
asserts.isequal(properties(odeJacobian()), jacobianProperties);
asserts.isequal(properties(odeDelay()), delayProperties);
asserts.isequal(properties(odeSensitivity()), sensitivityProperties);
asserts.isequal(properties(paramResult), resultProperties);
vectorProblem = ode('ODEFcn', @(t, y)[y(2); -y(1)], ...
  'InitialValue', [0; 1]);
vectorResult = solve(vectorProblem, 0, 0.5);
asserts.isequal(size(vectorResult.Time, 1), 1);
asserts.isequal(size(vectorResult.Solution, 1), 2);
asserts.isequal(size(vectorResult.Solution, 2), length(vectorResult.Time));

defaultMass = odeMassMatrix();
asserts.isequal(defaultMass.Singular, 'maybe');
asserts.isequal(defaultMass.StateDependence, 'weak');
constantMass = odeMassMatrix(2);
zeroMass = odeMassMatrix(0);
singularMass = odeMassMatrix([1 0; 0 0]);
asserts.isequal(constantMass.Singular, 'no');
asserts.isequal(constantMass.StateDependence, 'none');
asserts.isequal(zeroMass.Singular, 'yes');
asserts.isequal(singularMass.Singular, 'yes');

mass = odeMassMatrix('MassMatrix', 1, 'Singular', 'no', ...
  'StateDependence', 'none', 'SparsityPattern', 1);
jac = odeJacobian('Jacobian', -1, 'SparsityPattern', 1);
event = odeEvent('EventFcn', @(t, y) y - 0.5, ...
  'Direction', 'increasing', 'Response', 'stop');
objectProblem = ode('ODEFcn', @(t, y) 1, ...
  'InitialValue', 0, ...
  'MassMatrix', mass, ...
  'Jacobian', jac, ...
  'EventDefinition', event);
objectResult = solve(objectProblem, 0, 1);
asserts.istrue(abs(objectResult.EventTime(1) - 0.5) < 1e-3);
asserts.istrue(abs(objectResult.EventSolution(1) - 0.5) < 1e-8);

legacyMass = odeMassMatrix(2, 'MassSingular', 'no', 'MStateDependence', 'none', 'MvPattern', 1);
legacyJac = odeJacobian(-1, 'Pattern', 1, 'Constant', 'on');
asserts.isequal(legacyMass.Singular, 'no');
asserts.isequal(legacyMass.StateDependence, 'none');
asserts.isequal(legacyMass.SparsityPattern, 1);
asserts.isequal(legacyJac.SparsityPattern, 1);
asserts.checkerror('odeEvent(''Direction'', ''bad'')', sprintf(_('%s has an invalid value.'), 'Direction'));
asserts.checkerror('odeEvent(''Response'', ''bad'')', sprintf(_('%s has an invalid value.'), 'Response'));
asserts.checkerror('odeDelay(''Unknown'', 1)', sprintf(_('Unknown %s property.'), 'delay'));
asserts.checkerror('odeSensitivity(''Unknown'', 1)', sprintf(_('Unknown %s property.'), 'sensitivity'));
validDelay = odeDelay('History', @(t) 1, 'ValueDelay', @(t, y) 0.1, ...
  'SlopeDelay', @(t, y, p) 0.2);
asserts.istrue(isa(validDelay.History, 'function_handle'));
asserts.checkerror('odeDelay(''History'', Inf)', _('History must be numeric, a function handle, or a solution structure.'));
asserts.checkerror('odeDelay(''ValueDelay'', NaN)', sprintf(_('%s must be numeric or a function handle.'), 'ValueDelay'));
asserts.checkerror('odeDelay(''SlopeDelay'', NaN)', sprintf(_('%s must be numeric or a function handle.'), 'SlopeDelay'));
asserts.checkerror('odeDelay(''History'', @odeDelayThreeInputFcn)', sprintf(_('%s function has an invalid signature.'), 'History'));
asserts.checkerror('odeDelay(''ValueDelay'', @odeDelayOneInputFcn)', sprintf(_('%s function has an invalid signature.'), 'ValueDelay'));
asserts.checkerror('odeDelay(''SlopeDelay'', @odeDelayOneInputFcn)', sprintf(_('%s function has an invalid signature.'), 'SlopeDelay'));
asserts.isequal(odeSensitivity('Method', 'forward').Method, 'direct');
asserts.checkerror('odeSensitivity(''Method'', ''bad'')', _('Sensitivity method must be direct or adjoint.'));
asserts.checkerror('odeSensitivity(''ParameterIndices'', Inf)', _('ParameterIndices must contain positive integer values.'));
asserts.checkerror('odeSensitivity(''ObjectiveFcn'', 1)', sprintf(_('%s must be a function handle.'), 'ObjectiveFcn'));
asserts.checkerror('odeSensitivity(''QuadratureFcn'', 1)', sprintf(_('%s must be a function handle.'), 'QuadratureFcn'));
asserts.checkerror('odeSensitivity(''ObjectiveTime'', Inf)', sprintf(_('%s must be a finite scalar.'), 'ObjectiveTime'));
asserts.checkerror('odeSensitivity(''AdjointRelativeTolerance'', 0)', ...
  sprintf(_('%s must be a positive finite scalar.'), 'AdjointRelativeTolerance'));
asserts.checkerror('odeSensitivity(''AdjointAbsoluteTolerance'', -1)', ...
  sprintf(_('%s must be a positive finite scalar.'), 'AdjointAbsoluteTolerance'));
%=============================================================================
callbackEvent = odeEvent('EventFcn', @(t, y) y - 0.5, ...
  'Direction', 'increasing', ...
  'Response', 'callback', ...
  'CallbackFcn', @odeTestEventCallbackStop);
callbackProblem = ode('ODEFcn', @(t, y) 1, ...
  'InitialValue', 0, ...
  'EventDefinition', callbackEvent);
callbackResult = solve(callbackProblem, 0, 1);
asserts.istrue(abs(callbackResult.Time(length(callbackResult.Time)) - 0.5) < 1e-8);
asserts.istrue(abs(callbackResult.SolutionValues(length(callbackResult.Time)) - 0.75) < 1e-8);
asserts.istrue(abs(callbackResult.EventSolution(1) - 0.75) < 1e-8);

proceedCallbackEvent = odeEvent('EventFcn', @(t, y) y - 0.5, ...
  'Direction', 'increasing', ...
  'Response', 'callback', ...
  'CallbackFcn', @odeTestEventCallbackProceed);
proceedCallbackProblem = ode('ODEFcn', @(t, y) 1, ...
  'InitialValue', 0, ...
  'EventDefinition', proceedCallbackEvent);
proceedCallbackResult = solve(proceedCallbackProblem, 0, 0.75);
asserts.istrue(abs(proceedCallbackResult.Time(length(proceedCallbackResult.Time)) - 0.75) < 1e-8);
asserts.istrue(abs(proceedCallbackResult.EventTime(1) - 0.5) < 1e-8);
asserts.istrue(abs(proceedCallbackResult.EventSolution(1) - 0.6) < 1e-8);

parameterCallbackEvent = odeEvent('EventFcn', @(t, y, rate, delta) y - 0.5, ...
  'Response', 'callback', ...
  'CallbackFcn', @odeTestEventCallbackWithParameters);
parameterCallbackProblem = ode('ODEFcn', @(t, y, rate, delta) rate, ...
  'InitialValue', 0, ...
  'Parameters', {1, 0.3}, ...
  'EventDefinition', parameterCallbackEvent);
parameterCallbackResult = solve(parameterCallbackProblem, 0, 1);
asserts.istrue(abs(parameterCallbackResult.Time(length(parameterCallbackResult.Time)) - 0.5) < 1e-8);
asserts.istrue(abs(parameterCallbackResult.EventSolution(1) - 0.8) < 1e-8);

missingCallbackEvent = odeEvent('EventFcn', @(t, y) y - 0.5, ...
  'Response', 'callback');
missingCallbackProblem = ode('ODEFcn', @(t, y) 1, ...
  'InitialValue', 0, ...
  'EventDefinition', missingCallbackEvent);
asserts.checkerror('solve(missingCallbackProblem, 0, 0.6);', sprintf(_('%s function handle expected.'), 'Callback'));

badStopCallbackEvent = odeEvent('EventFcn', @(t, y) y - 0.5, ...
  'Response', 'callback', ...
  'CallbackFcn', @odeTestEventCallbackBadStop);
badStopCallbackProblem = ode('ODEFcn', @(t, y) 1, ...
  'InitialValue', 0, ...
  'EventDefinition', badStopCallbackEvent);
asserts.checkerror('solve(badStopCallbackProblem, 0, 0.6);', ...
  sprintf(_('%s stop flag must be a scalar numeric or logical value.'), 'Callback'));
%=============================================================================
