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
    error('Nelson:control_system:unknownProperty', sprintf(msg, 'ss', name));
  end
  if length(s) > 1
    currentValue = ltiSubscriptGet(obj, name);
    value = subsasgn(currentValue, s(2:end), value);
  end
  switch name
    case 'Ts'
      obj.Ts = ltiValidateSampleTime(value);
      obj.Internal.Ts = obj.Ts;
      obj.Internal.IsStatic = false;
    case 'TimeUnit'
      obj.TimeUnit = ltiValidateTimeUnit(value);
    case 'Name'
      obj.Name = ltiValidateTextScalar('Name', value);
    otherwise
      obj = ltiSubscriptSet(obj, name, value);
  end
  varargout{1} = obj;
end
%=============================================================================
function obj = assignSubsystem(obj, indices, value)
  if length(indices) ~= 2
    error('Nelson:control_system:invalidSubscript', '%s expected.', 'Two indices');
  end
  value = ss(value);
  ltiCheckSampleTimeCompatibility(obj, value);
  outputs = indices{1};
  inputs = indices{2};
  D = obj.D;
  D(outputs, inputs) = value.D;
  obj.D = D;
  if isempty(obj.A) && isempty(value.A)
    return
  end
  if ~isequal(obj.A, value.A)
    if isempty(value.A)
      return
    end
    error('Nelson:control_system:invalidAssignment', 'State-space assignment must preserve the state matrix.');
  end
  B = obj.B;
  C = obj.C;
  if ~isempty(B) && ~isempty(value.B)
    B(:, inputs) = value.B;
    obj.B = B;
  end
  if ~isempty(C) && ~isempty(value.C)
    C(outputs, :) = value.C;
    obj.C = C;
  end
end
%=============================================================================
