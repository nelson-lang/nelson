%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = subsref(varargin)
  obj = varargin{1};
  s = varargin{2};
  if strcmp(s(1).type, '()')
    if length(s) > 1
      partial = subsref(obj, s(1));
      [varargout{1:nargout}] = subsref(partial, s(2:end));
      return
    end
    indices = s(1).subs;
    z = subscriptGet(obj, 'Z');
    p = subscriptGet(obj, 'P');
    k = subscriptGet(obj, 'K');
    sampleTime = subscriptGet(obj, 'Ts');
    sys = zpk(z(indices{:}), p(indices{:}), k(indices{:}), sampleTime);
    sys.Variable = subscriptGet(obj, 'Variable');
    sys.TimeUnit = subscriptGet(obj, 'TimeUnit');
    sys.UserData = subscriptGet(obj, 'UserData');
    sys.InputName = lti.selectIOProperty(subscriptGet(obj, 'InputName'), indices, 2);
    sys.InputUnit = lti.selectIOProperty(subscriptGet(obj, 'InputUnit'), indices, 2);
    sys.OutputName = lti.selectIOProperty(subscriptGet(obj, 'OutputName'), indices, 1);
    sys.OutputUnit = lti.selectIOProperty(subscriptGet(obj, 'OutputUnit'), indices, 1);
    varargout{1} = sys;
    return
  end
  if strcmp(s(1).type, '{}')
    error('Nelson:control_system:invalidSubscript:illegalIndexingStructureArgumentTypeValue', 'Illegal indexing structure argument: type %s expected.', "'.' or '()'");
  end
  if ~strcmp(s(1).type, '.')
    error('Nelson:control_system:invalidSubscript:notAValidIndexingExpression', 'Not a valid indexing expression');
  end
  value = subscriptGet(obj, s(1).subs);
  if length(s) > 1
    [varargout{1:nargout}] = subsref(value, s(2:end));
  else
    varargout{1} = value;
  end
end
%=============================================================================
