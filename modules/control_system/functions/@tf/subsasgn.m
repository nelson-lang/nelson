%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = subsasgn(varargin)
  obj = varargin{1};
  s = varargin{2};
  value = varargin{3};
  if strcmp(s(1).type, '()')
    if length(s) > 1
      currentValue = subsref(obj, s(1));
      value = subsasgn(currentValue, s(2:end), value);
    end
    obj = assignSubsystem(obj, s(1).subs, value);
    varargout{1} = obj;
    return
  end
  if strcmp(s(1).type, '{}')
    error('Nelson:control_system:invalidSubscript:illegalIndexingStructureArgumentTypeValue', 'Illegal indexing structure argument: type %s expected.', "'.' or '()'");
  end
  if ~strcmp(s(1).type, '.')
    error('Nelson:control_system:invalidSubscript:notAValidIndexingExpression', 'Not a valid indexing expression');
  end
  name = s(1).subs;
  if ~isprop(obj, name)
    msg = _('No property of the class ''%s'' matches the identifier ''%s''.');
    error('Nelson:control_system:unknownProperty', sprintf(msg, 'tf', name));
  end
  if length(s) > 1
    currentValue = subscriptGet(obj, name);
    value = subsasgn(currentValue, s(2:end), value);
  end
  switch name
    case 'Numerator'
      if ~iscell(value)
        value = {value};
      end
      obj.Numerator = value;
    case 'Denominator'
      if ~iscell(value)
        value = {value};
      end
      obj.Denominator = value;
    case 'Variable'
      obj.Variable = validateVariable(obj, value);
    case 'Ts'
      obj.Ts = lti.validateSampleTime(value);
      obj.Internal.Ts = obj.Ts;
      obj.Internal.IsStatic = false;
    case 'TimeUnit'
      obj.TimeUnit = lti.validateTimeUnit(value);
    case 'Name'
      obj.Name = lti.validateTextScalar('Name', value);
    otherwise
      obj = subscriptSet(obj, name, value);
  end
  varargout{1} = obj;
end
%=============================================================================
function obj = assignSubsystem(obj, indices, value)
  value = tf(value);
  lti.checkSampleTimeCompatibility(obj, value);
  numerator = obj.Numerator;
  denominator = obj.Denominator;
  replacementNumerator = expandReplacement(value.Numerator, numerator(indices{:}));
  replacementDenominator = expandReplacement(value.Denominator, denominator(indices{:}));
  numerator(indices{:}) = replacementNumerator;
  denominator(indices{:}) = replacementDenominator;
  obj.Numerator = numerator;
  obj.Denominator = denominator;
  if ~isempty(value.Variable)
    obj.Variable = value.Variable;
  end
end
%=============================================================================
function replacement = expandReplacement(replacement, target)
  if isequal(size(replacement), size(target))
    return
  end
  if isscalar(replacement)
    scalarValue = replacement{1};
    replacement = cell(size(target));
    replacement(:) = {scalarValue};
    return
  end
  error('Nelson:control_system:dimagree:matrixDimensionsAgree', 'Matrix dimensions must agree.');
end
function value = validateVariable(obj, value)
  value = lti.validateTextScalar('Variable', value);
  supported = {'s', 'p', 'z', 'q', 'z^-1', 'q^-1'};
  if ~any(strcmp(value, supported))
    error('Nelson:control_system:invalidVariable', '%s expected.', "'s', 'p', 'z', 'q', 'z^-1', or 'q^-1'");
  end
  if obj.Ts == 0 && any(strcmp(value, {'z', 'q', 'z^-1', 'q^-1'})) && ~isstatic(obj)
    error('Nelson:control_system:invalidVariable:discreteTimeVariableRequiresADiscrete', 'Discrete-time variable requires a discrete-time model.');
  end
  if (obj.Ts > 0 || obj.Ts == -1) && any(strcmp(value, {'s', 'p'}))
    error('Nelson:control_system:invalidVariable:continuousTimeVariableRequiresAContinuous', 'Continuous-time variable requires a continuous-time model.');
  end
end
%=============================================================================
