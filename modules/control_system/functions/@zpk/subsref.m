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
    z = ltiSubscriptGet(obj, 'Z');
    p = ltiSubscriptGet(obj, 'P');
    k = ltiSubscriptGet(obj, 'K');
    sampleTime = ltiSubscriptGet(obj, 'Ts');
    sys = zpk(z(indices{:}), p(indices{:}), k(indices{:}), sampleTime);
    sys.Variable = ltiSubscriptGet(obj, 'Variable');
    sys.TimeUnit = ltiSubscriptGet(obj, 'TimeUnit');
    sys.UserData = ltiSubscriptGet(obj, 'UserData');
    sys.InputName = ltiSelectIOProperty(ltiSubscriptGet(obj, 'InputName'), indices, 2);
    sys.InputUnit = ltiSelectIOProperty(ltiSubscriptGet(obj, 'InputUnit'), indices, 2);
    sys.OutputName = ltiSelectIOProperty(ltiSubscriptGet(obj, 'OutputName'), indices, 1);
    sys.OutputUnit = ltiSelectIOProperty(ltiSubscriptGet(obj, 'OutputUnit'), indices, 1);
    varargout{1} = sys;
    return
  end
  if strcmp(s(1).type, '{}')
    error('Nelson:control_system:invalidSubscript:illegalIndexingStructureArgumentTypeValue', 'Illegal indexing structure argument: type %s expected.', "'.' or '()'");
  end
  if ~strcmp(s(1).type, '.')
    error('Nelson:control_system:invalidSubscript:notAValidIndexingExpression', 'Not a valid indexing expression');
  end
  value = ltiSubscriptGet(obj, s(1).subs);
  if length(s) > 1
    [varargout{1:nargout}] = subsref(value, s(2:end));
  else
    varargout{1} = value;
  end
end
%=============================================================================
