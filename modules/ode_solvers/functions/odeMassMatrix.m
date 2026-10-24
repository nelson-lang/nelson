%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef odeMassMatrix
  properties
    MassMatrix = []
    Singular = 'maybe'
    StateDependence = 'weak'
    SparsityPattern = []
  end

  methods
    function obj = odeMassMatrix(varargin)
      startIndex = 1;
      hasMassMatrix = false;
      hasSingular = false;
      hasStateDependence = false;
      if nargin > 0 && ~ischar(varargin{1}) && ~isstring(varargin{1})
        value = varargin{1};
        if ~(isempty(value) || isnumeric(value) || isa(value, 'function_handle'))
          error('Nelson:ode_solvers:invalidType:valueNumericAFunctionHandle', '%s must be numeric or a function handle.', 'Mass matrix');
        end
        obj.MassMatrix = value;
        hasMassMatrix = true;
        startIndex = 2;
      end
      if mod(nargin - startIndex + 1, 2) ~= 0
        error('Nelson:ode_solvers:nameValueArgumentsExpected', 'Name-value arguments expected.');
      end
      for k = startIndex:2:nargin
        name = char(varargin{k});
        lname = lower(name);
        hasMassMatrix = hasMassMatrix || strcmp(lname, 'massmatrix');
        hasSingular = hasSingular || strcmp(lname, 'singular') || strcmp(lname, 'masssingular');
        hasStateDependence = hasStateDependence ...
          || strcmp(lname, 'statedependence') || strcmp(lname, 'mstatedependence');
        obj = odeAssignMassProperty(obj, name, varargin{k + 1});
      end
      obj = odeApplyMassDefaults(obj, hasMassMatrix, hasSingular, hasStateDependence);
    end

    function obj = set.MassMatrix(obj, value)
      if ~(isempty(value) || isnumeric(value) || isa(value, 'function_handle'))
        error('Nelson:ode_solvers:invalidType:valueNumericAFunctionHandle', '%s must be numeric or a function handle.', 'Mass matrix');
      end
      obj.MassMatrix = value;
    end

    function obj = set.Singular(obj, value)
      obj.Singular = odeValidateMassChoice(value, 'Singular', {'yes', 'no', 'maybe'});
    end

    function obj = set.StateDependence(obj, value)
      obj.StateDependence = odeValidateMassChoice(value, 'StateDependence', {'none', 'weak', 'strong'});
    end

    function obj = set.SparsityPattern(obj, value)
      odeValidateMassPattern(value);
      obj.SparsityPattern = value;
    end
  end
end
%=============================================================================
function obj = odeApplyMassDefaults(obj, hasMassMatrix, hasSingular, hasStateDependence)
  if ~hasMassMatrix || isempty(obj.MassMatrix)
    return
  end
  if isnumeric(obj.MassMatrix)
    if ~hasStateDependence
      obj.StateDependence = 'none';
    end
    if ~hasSingular
      obj.Singular = odeMassSingularDefault(obj.MassMatrix);
    end
  elseif isa(obj.MassMatrix, 'function_handle') && ~hasStateDependence
    if nargin(obj.MassMatrix) <= 1
      obj.StateDependence = 'none';
    end
  end
end
%=============================================================================
function value = odeMassSingularDefault(M)
  if isscalar(M)
    if M == 0
      value = 'yes';
    else
      value = 'no';
    end
    return
  end
  if size(M, 1) ~= size(M, 2)
    value = 'maybe';
  elseif rank(M) < size(M, 2)
    value = 'yes';
  else
    value = 'no';
  end
end
%=============================================================================
function obj = odeAssignMassProperty(obj, name, value)
  switch lower(name)
    case {'singular', 'masssingular'}
      obj.Singular = odeValidateMassChoice(value, 'MassSingular', {'yes', 'no', 'maybe'});
    case {'statedependence', 'mstatedependence'}
      obj.StateDependence = odeValidateMassChoice(value, 'MStateDependence', {'none', 'weak', 'strong'});
    case {'sparsitypattern', 'mvpattern'}
      odeValidateMassPattern(value);
      obj.SparsityPattern = value;
    case 'massmatrix'
      obj.MassMatrix = value;
    otherwise
      error('Nelson:ode_solvers:unknownMassMatrixProperty', 'Unknown %s property.', 'mass matrix');
  end
end
%=============================================================================
function value = odeValidateMassChoice(value, name, choices)
  if isempty(value)
    value = [];
    return
  end
  if isstring(value)
    value = char(value);
  end
  if ~ischar(value)
    error('Nelson:ode_solvers:invalidValue', '%s has an invalid value.', name);
  end
  valid = false;
  for k = 1:length(choices)
    if strcmpi(value, choices{k})
      value = choices{k};
      valid = true;
      break
    end
  end
  if ~valid
    error('Nelson:ode_solvers:invalidValue', '%s has an invalid value.', name);
  end
end
%=============================================================================
function odeValidateMassPattern(value)
  if ~(isempty(value) || isnumeric(value) || islogical(value))
    error('Nelson:validators:mustBeNumericOrLogical', '%s must be numeric or logical.', 'Mass vector pattern');
  end
end
%=============================================================================
