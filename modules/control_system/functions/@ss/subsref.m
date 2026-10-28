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
    A = subscriptGet(obj, 'A');
    B = subscriptGet(obj, 'B');
    C = subscriptGet(obj, 'C');
    D = subscriptGet(obj, 'D');
    if ~isempty(B)
      B = B(:, inputs);
    end
    if ~isempty(C)
      C = C(outputs, :);
    end
    if ~isempty(D)
      D = D(outputs, inputs);
    end
    sys = ss(A, B, C, D, subscriptGet(obj, 'Ts'));
    sys.E = subscriptGet(obj, 'E');
    sys.Scaled = subscriptGet(obj, 'Scaled');
    sys.TimeUnit = subscriptGet(obj, 'TimeUnit');
    sys.UserData = subscriptGet(obj, 'UserData');
    sys.InputName = lti.selectIOProperty(subscriptGet(obj, 'InputName'), inputs);
    sys.InputUnit = lti.selectIOProperty(subscriptGet(obj, 'InputUnit'), inputs);
    sys.OutputName = lti.selectIOProperty(subscriptGet(obj, 'OutputName'), outputs);
    sys.OutputUnit = lti.selectIOProperty(subscriptGet(obj, 'OutputUnit'), outputs);
    varargout{1} = sys;
    return
  end
  if strcmp(s(1).type, '{}')
    error('Nelson:control_system:invalidSubscript:illegalIndexingStructureArgumentTypeValue', 'Illegal indexing structure argument: type %s expected.', "'.'");
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
