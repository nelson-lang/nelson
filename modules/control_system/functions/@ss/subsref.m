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
    if length(indices) ~= 2
      error('Nelson:control_system:invalidSubscript', '%s expected.', 'Two indices');
    end
    outputs = indices{1};
    inputs = indices{2};
    A = ltiSubscriptGet(obj, 'A');
    B = ltiSubscriptGet(obj, 'B');
    C = ltiSubscriptGet(obj, 'C');
    D = ltiSubscriptGet(obj, 'D');
    if ~isempty(B)
      B = B(:, inputs);
    end
    if ~isempty(C)
      C = C(outputs, :);
    end
    if ~isempty(D)
      D = D(outputs, inputs);
    end
    sys = ss(A, B, C, D, ltiSubscriptGet(obj, 'Ts'));
    sys.E = ltiSubscriptGet(obj, 'E');
    sys.Scaled = ltiSubscriptGet(obj, 'Scaled');
    sys.TimeUnit = ltiSubscriptGet(obj, 'TimeUnit');
    sys.UserData = ltiSubscriptGet(obj, 'UserData');
    sys.InputName = ltiSelectIOProperty(ltiSubscriptGet(obj, 'InputName'), inputs);
    sys.InputUnit = ltiSelectIOProperty(ltiSubscriptGet(obj, 'InputUnit'), inputs);
    sys.OutputName = ltiSelectIOProperty(ltiSubscriptGet(obj, 'OutputName'), outputs);
    sys.OutputUnit = ltiSelectIOProperty(ltiSubscriptGet(obj, 'OutputUnit'), outputs);
    varargout{1} = sys;
    return
  end
  if strcmp(s(1).type, '{}')
    error('Nelson:control_system:invalidSubscript:illegalIndexingStructureArgumentTypeValue', 'Illegal indexing structure argument: type %s expected.', "'.'");
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
