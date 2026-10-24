%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function options = odeOptionsFromObject(obj)
  options = odeDefaultOptions();
  options = odeMergeOptions(options, obj.SolverOptions);
  options.RelTol = obj.RelativeTolerance;
  options.AbsTol = obj.AbsoluteTolerance;
  options.NonNegative = obj.NonNegativeVariables;
  options.InitialSlope = obj.InitialSlope;
  options.Parameters = obj.Parameters;
  if ~isempty(obj.MassMatrix)
    options.Mass = obj.MassMatrix.MassMatrix;
    options.MassSingular = obj.MassMatrix.Singular;
    options.MStateDependence = obj.MassMatrix.StateDependence;
    options.MvPattern = obj.MassMatrix.SparsityPattern;
  end
  if ~isempty(obj.Jacobian)
    options.Jacobian = obj.Jacobian.Jacobian;
    options.JPattern = obj.Jacobian.SparsityPattern;
    options.JConstant = obj.Jacobian.Constant;
  end
  if ~isempty(obj.EventDefinition)
    options.Events = @odeEvaluateEventObject;
    options.EventDefinitionObject = obj.EventDefinition;
  end
  if isprop(obj.SolverOptions, 'ComputeConsistentInitialConditions')
    options.ComputeConsistentInitialConditions = obj.SolverOptions.ComputeConsistentInitialConditions;
  end
end
%=============================================================================
