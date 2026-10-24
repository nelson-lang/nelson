%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
problemProperties = {'EquationType'; 'ODEFcn'; 'Parameters'; 'InitialTime'; ...
  'InitialValue'; 'InitialSlope'; 'Jacobian'; 'MassMatrix'; ...
  'NonNegativeVariables'; 'EventDefinition'; 'DelayDefinition'; 'Sensitivity'; ...
  'AbsoluteTolerance'; 'RelativeTolerance'; 'Solver'; 'SolverOptions'; ...
  'SeparateComplexParts'; 'SelectedSolver'};
asserts.isequal(properties(ode()), problemProperties);
previousSundialsRuntime = getenv('NELSON_SUNDIALS_RUNTIME');
runtimeCleanup = onCleanup(@() setenv('NELSON_SUNDIALS_RUNTIME', previousSundialsRuntime)); %#ok<NLS0004>
setenv('NELSON_SUNDIALS_RUNTIME', 'OFF');
defaultProblem = ode();
asserts.isequal(defaultProblem.Solver, 'auto');
asserts.isequal(defaultProblem.SelectedSolver, 'ode45');
asserts.isequal(defaultProblem.SolverOptions.ID, 'ode45');
implicitDefaultProblem = ode('EquationType', 'fullyimplicit');
asserts.isequal(implicitDefaultProblem.Solver, 'auto');
asserts.isequal(implicitDefaultProblem.SelectedSolver, 'ode15i');
asserts.isequal(implicitDefaultProblem.SolverOptions.ID, 'ode15i');
explicitSolverProblem = ode('Solver', 'stiff');
asserts.isequal(explicitSolverProblem.Solver, 'stiff');
asserts.isequal(explicitSolverProblem.SelectedSolver, 'ode15s');
asserts.isequal(explicitSolverProblem.SolverOptions.ID, 'ode15s');
setenv('NELSON_SUNDIALS_RUNTIME', previousSundialsRuntime);
publicFunctions = {'bvp4c'; 'bvp5c'; 'bvpget'; 'bvpinit'; 'bvpset'; 'bvpxtend'; ...
  'decic'; 'dde23'; 'ddeget'; 'ddensd'; 'ddesd'; 'ddeset'; 'deval'; ...
  'ode'; 'ode113'; 'ode15i'; 'ode15s'; 'ode23'; 'ode23s'; 'ode23t'; ...
  'ode23tb'; 'ode45'; 'ode78'; 'ode89'; 'odeDelay'; 'odeEvent'; ...
  'odeexamples'; 'odeget'; 'odeJacobian'; 'odeMassMatrix'; 'odephas2'; ...
  'odephas3'; 'odeplot'; 'odeprint'; 'odeSensitivity'; 'odeset'; 'odextend'};
publicFunctions = sort(publicFunctions);
actualFunctions = dir([modulepath('ode_solvers', 'functions'), '/*.m']);
actualFunctions = {actualFunctions.name}';
for k = 1:length(actualFunctions)
  actualFunctions{k} = strrep(actualFunctions{k}, '.m', '');
end
actualFunctions = sort(actualFunctions);
asserts.isequal(actualFunctions, publicFunctions);
asserts.isequal(exist('ODEResults'), 0);
asserts.isequal(exist('nelson.ode.ODEResults'), 8);
asserts.isequal(exist('Options'), 0);
asserts.isequal(exist('nelson.ode.Options'), 8);
asserts.isequal(exist('ODE45'), 0);
asserts.isequal(exist('nelson.ode.options.ODE45'), 8);
solverOptionProperties = {'InitialStep'; 'MaxStep'; 'MinStep'; ...
  'NormControl'; 'OutputFcn'; 'OutputSelection'};
stiffOptionProperties = {'InitialStep'; 'MaxStep'; 'MinStep'; ...
  'NormControl'; 'OutputFcn'; 'OutputSelection'; 'Vectorization'};
asserts.isequal(properties(nelson.ode.options.ODE45()), solverOptionProperties);
asserts.isequal(properties(nelson.ode.options.ODE23s()), stiffOptionProperties);
asserts.isequal(properties(nelson.ode.options.CVODESNonstiff()), ...
  {'InitialStep'; 'MaxStep'; 'MinStep'; 'NormControl'; 'OutputFcn'; ...
  'OutputSelection'; 'Vectorization'; 'MaxOrder'; 'LinearSolver'; 'Preconditioner'});
asserts.isequal(properties(nelson.ode.options.IDAS()), ...
  {'InitialStep'; 'MaxStep'; 'MinStep'; 'NormControl'; 'OutputFcn'; ...
  'OutputSelection'; 'Vectorization'; 'MaxOrder'; 'ComputeConsistentInitialConditions'; ...
  'LinearSolver'; 'Preconditioner'});
asserts.isequal(exist('ODESolverOptions'), 0);
privateHelpers = {'odeValidateMassChoice'; 'odeValidateJacobianConstant'; ...
  'odeIsSolverOptions'; 'odeMakeSolverOptions'; 'odeOutputPlot'};
for k = 1:length(privateHelpers)
  asserts.isequal(exist(privateHelpers{k}), 0);
end
%=============================================================================
options = nelson.ode.options.ODE23('MaxStep', 0.25, ...
  'OutputSelection', 1, 'NormControl', "ON");
optionStruct = struct(options);
asserts.isequal(options.ID, 'ode23');
asserts.isfalse(isDefault(options));
asserts.isequal(options.DefaultRefine, 1);
asserts.isequal(optionStruct.ID, 'ode23');
asserts.isequal(optionStruct.MaxStep, 0.25);
asserts.isequal(optionStruct.OutputSel, 1);
asserts.isequal(optionStruct.NormControl, 'on');
asserts.isequal(optionStruct.Refine, 1);
structOptions = nelson.ode.options.ODE45(odeset('MaxStep', 0.5, 'Refine', 3));
asserts.isequal(structOptions.ID, 'ode45');
asserts.isfalse(isDefault(structOptions));
asserts.isequal(struct(structOptions).MaxStep, 0.5);
asserts.isequal(struct(structOptions).Refine, 3);
objectOptionsProblem = ode('SolverOptions', ...
  nelson.ode.options.ODE15s(odeset('MaxStep', 0.125, 'InitialStep', 0.01)));
asserts.isequal(objectOptionsProblem.Solver, 'ode15s');
asserts.isequal(objectOptionsProblem.SelectedSolver, 'ode15s');
asserts.isequal(objectOptionsProblem.SolverOptions.ID, 'ode15s');
asserts.isequal(struct(objectOptionsProblem.SolverOptions).MaxStep, 0.125);
asserts.isequal(struct(objectOptionsProblem.SolverOptions).InitialStep, 0.01);
asserts.checkerror('ode(''SolverOptions'', odeset(''MaxStep'', 0.125))', ...
  _('Solver options object expected.'));
asserts.checkerror('ode(''Solver'', ''stiff'', ''SolverOptions'', nelson.ode.options.ODE15s())', ...
  _('Specified solver (stiff) is incompatible with specified options (ode15s). Specify one or the other.'));
directOptionsProblem = ode();
asserts.checkerror('directOptionsProblem.SolverOptions = nelson.ode.options.ODE23();', ...
  _('SolverOptions property cannot be modified when the Solver property is "auto". Set the Solver property to "ode45" and try again.')); %#ok<NLS0012>
directOptionsProblem.Solver = 'ode23';
directOptionsProblem.SolverOptions = nelson.ode.options.ODE23('MaxStep', 0.3);
asserts.isequal(directOptionsProblem.Solver, 'ode23');
asserts.isequal(directOptionsProblem.SelectedSolver, 'ode23');
asserts.isequal(struct(directOptionsProblem.SolverOptions).MaxStep, 0.3);
directOptionsProblem.Solver = 'ode45';
directOptionsProblem.SolverOptions = nelson.ode.options.ODE23('MaxStep', 0.4);
asserts.isequal(directOptionsProblem.Solver, 'ode23');
asserts.isequal(directOptionsProblem.SelectedSolver, 'ode23');
asserts.isequal(struct(directOptionsProblem.SolverOptions).MaxStep, 0.4);
defaultOptions = nelson.ode.options.ODE89();
asserts.isequal(defaultOptions.ID, 'ode89');
asserts.isequal(defaultOptions.DefaultRefine, 8);
asserts.istrue(isDefault(defaultOptions));
%=============================================================================
savedProblem = ode('ODEFcn', @(t, y, rate) rate * y, ...
  'Parameters', {2}, ...
  'InitialTime', 0.25, ...
  'InitialValue', [1 + 1i; 2 - 1i], ...
  'InitialSlope', [0; 0], ...
  'Jacobian', odeJacobian(2), ...
  'MassMatrix', odeMassMatrix(eye(2)), ...
  'NonNegativeVariables', [1 2], ...
  'EventDefinition', odeEvent(@(t, y) y(1) - 2), ...
  'AbsoluteTolerance', 1e-8, ...
  'RelativeTolerance', 1e-5, ...
  'SolverOptions', options, ...
  'SeparateComplexParts', 'on');
saved = saveobj(savedProblem);
savedFields = {'EquationType'; 'ODEFcn'; 'Parameters'; 'InitialTime'; ...
  'InitialValue'; 'InitialSlope'; 'Jacobian'; 'MassMatrix'; ...
  'NonNegativeVariables'; 'EventDefinition'; 'DelayDefinition'; 'Sensitivity'; ...
  'AbsoluteTolerance'; 'RelativeTolerance'; 'Solver'; 'SolverOptions'; ...
  'SeparateComplexParts'; 'SelectedSolver'; 'Version'};
asserts.isequal(fieldnames(saved), savedFields);
loaded = ode.loadobj(saved);
asserts.isequal(loaded.EquationType, savedProblem.EquationType);
asserts.isequal(loaded.Parameters, savedProblem.Parameters);
asserts.isequal(loaded.InitialTime, savedProblem.InitialTime);
asserts.isequal(loaded.InitialValue, savedProblem.InitialValue);
asserts.isequal(loaded.InitialSlope, savedProblem.InitialSlope);
asserts.isequal(loaded.NonNegativeVariables, savedProblem.NonNegativeVariables);
asserts.isequal(loaded.AbsoluteTolerance, savedProblem.AbsoluteTolerance);
asserts.isequal(loaded.RelativeTolerance, savedProblem.RelativeTolerance);
asserts.isequal(loaded.Solver, savedProblem.Solver);
asserts.isequal(loaded.SelectedSolver, savedProblem.SelectedSolver);
asserts.isequal(loaded.SeparateComplexParts, 'on');
asserts.isequal(loaded.SolverOptions.ID, 'ode23');
asserts.isequal(struct(loaded.SolverOptions).MaxStep, 0.25);
%=============================================================================
mass = odeMassMatrix(eye(2), 'MassSingular', 'NO', ...
  'MStateDependence', "NONE", 'MvPattern', logical(eye(2)));
asserts.isequal(mass.Singular, 'no');
asserts.isequal(mass.StateDependence, 'none');
asserts.isequal(mass.SparsityPattern, logical(eye(2)));
functionMass = odeMassMatrix(@(t) eye(2));
asserts.isequal(functionMass.StateDependence, 'none');
stateFunctionMass = odeMassMatrix(@(t, y) eye(length(y)));
asserts.isequal(stateFunctionMass.StateDependence, 'weak');
%=============================================================================
jacobianCell = odeJacobian({eye(2), 2 * eye(2)}, ...
  'Pattern', {logical(eye(2)), logical(eye(2))}, ...
  'Constant', "ON");
asserts.isequal(jacobianCell.Jacobian{1}, eye(2));
asserts.isequal(jacobianCell.Jacobian{2}, 2 * eye(2));
asserts.isequal(jacobianCell.SparsityPattern{1}, logical(eye(2)));
asserts.isequal(jacobianCell.SparsityPattern{2}, logical(eye(2)));
asserts.isequal(jacobianCell.Constant, 'on');
asserts.checkerror('ode(''UnknownObjectProperty'', 1)', _('Invalid argument name ''UnknownObjectProperty''.'));
asserts.checkerror('ode(''SelectedSolver'', ''ode15s'')', _('Invalid argument name ''SelectedSolver''.'));
asserts.checkerror('ode(''Version'', 2)', _('Invalid argument name ''Version''.'));
asserts.checkerror('ode(''Solver'', ''badsolver'')', _('Unknown ODE solver.'));
asserts.checkerror('odeMassMatrix([], ''Unknown'', 1)', sprintf(_('Unknown %s property.'), 'mass matrix'));
asserts.checkerror('odeJacobian([], ''Unknown'', 1)', sprintf(_('Unknown %s property.'), 'Jacobian'));
%=============================================================================
