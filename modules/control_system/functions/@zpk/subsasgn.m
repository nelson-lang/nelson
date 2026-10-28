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
  if strcmp(s(1).type, '{}')
    error('Nelson:control_system:invalidSubscript:illegalIndexingStructureArgumentTypeValue', 'Illegal indexing structure argument: type %s expected.', "'.' or '()'");
  end
  if strcmp(s(1).type, '()')
    error('Nelson:control_system:invalidSubscript:notAValidIndexingExpression', 'Not a valid indexing expression');
  end
  if ~strcmp(s(1).type, '.')
    error('Nelson:control_system:invalidSubscript:notAValidIndexingExpression', 'Not a valid indexing expression');
  end
  name = s(1).subs;
  if ~isprop(obj, name)
    msg = _('No property of the class ''%s'' matches the identifier ''%s''.');
    error('Nelson:control_system:unknownProperty', sprintf(msg, 'zpk', name));
  end
  if length(s) > 1
    currentValue = subscriptGet(obj, name);
    value = subsasgn(currentValue, s(2:end), value);
  end
  switch name
    case 'Z'
      obj.Z = value;
    case 'P'
      obj.P = value;
    case 'K'
      obj.K = value;
    case 'Variable'
      obj.Variable = value;
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
