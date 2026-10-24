%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [odefun, y0, yp0, options, restore] = odeSeparateComplexParts(solver, odefun, y0, yp0, options)
  n = length(y0(:));
  restore = struct('StateSize', n, 'Options', options);
  y0 = odeComplexToRealState(y0);
  yp0 = odeComplexToRealOptionalState(yp0);
  options = odeSeparateComplexOptions(options, n);
  isImplicit = strcmp(solver, 'ode15i') || strcmp(solver, 'idas');
  originalFcn = odefun;
  odefun = @(t, y, varargin) odeSeparatedComplexOdeFcn(originalFcn, n, isImplicit, t, y, varargin{:});
end
%=============================================================================
function options = odeSeparateComplexOptions(options, n)
  options.AbsTol = odeDuplicateStateOption(options.AbsTol, n);
  options.InitialSlope = odeComplexToRealOptionalState(options.InitialSlope);
  options.Mass = odeSeparateComplexMatrixOption(options.Mass, n);
  options.Jacobian = odeSeparateComplexMatrixOption(options.Jacobian, n);
  options.JPattern = odeDuplicatePattern(options.JPattern, n);
  options.MvPattern = odeDuplicatePattern(options.MvPattern, n);
  options.NonNegative = options.NonNegative(:);
  if ~isempty(options.OutputSel)
    originalOutputSel = options.OutputSel(:);
    originalOutputFcn = options.OutputFcn;
    options.OutputFcn = @(t, y, flag, varargin) odeSeparatedComplexOutputFcn(...
      originalOutputFcn, n, originalOutputSel, t, y, flag, varargin{:});
    options.OutputSel = [];
  elseif ~isempty(options.OutputFcn)
    originalOutputFcn = options.OutputFcn;
    options.OutputFcn = @(t, y, flag, varargin) odeSeparatedComplexOutputFcn(...
      originalOutputFcn, n, [], t, y, flag, varargin{:});
  end
  if isfield(options, 'EventDefinitionObject')
    options.EventDefinitionObject = odeSeparateComplexEventObject(options.EventDefinitionObject, n);
  elseif ~isempty(options.Events)
    originalEvents = options.Events;
    options.Events = @(t, y, varargin) odeSeparatedComplexEventFcn(originalEvents, n, t, y, varargin{:});
  end
end
%=============================================================================
function z = odeComplexToRealOptionalState(y)
  if isempty(y)
    z = y;
  else
    z = odeComplexToRealState(y);
  end
end
%=============================================================================
function value = odeDuplicateStateOption(value, n)
  if isempty(value) || isscalar(value)
    return
  end
  value = value(:);
  if length(value) == n
    value = [value; value];
  end
end
%=============================================================================
function value = odeSeparateComplexMatrixOption(value, n)
  if isempty(value)
    return
  end
  if isa(value, 'function_handle')
    originalValue = value;
    value = @(t, y, varargin) odeComplexMatrixBlock(...
      originalValue(t, odeRealToComplexState(y, n), varargin{:}), n);
  else
    value = odeComplexMatrixBlock(value, n);
  end
end
%=============================================================================
function pattern = odeDuplicatePattern(pattern, n)
  if isempty(pattern)
    return
  end
  if isequal(size(pattern), [n n])
    pattern = [pattern pattern; pattern pattern];
  end
end
%=============================================================================
function eventDefinition = odeSeparateComplexEventObject(eventDefinition, n)
  if ~isempty(eventDefinition.EventFcn)
    originalEventFcn = eventDefinition.EventFcn;
    eventDefinition.EventFcn = @(t, y, varargin) odeSeparatedComplexEventFcn(...
      originalEventFcn, n, t, y, varargin{:});
  end
  if ~isempty(eventDefinition.CallbackFcn)
    originalCallback = eventDefinition.CallbackFcn;
    eventDefinition.CallbackFcn = @(t, y, index, varargin) odeSeparatedComplexCallback(...
      originalCallback, n, t, y, index, varargin{:});
  end
end
%=============================================================================
function dy = odeSeparatedComplexOdeFcn(fcn, n, isImplicit, t, y, varargin)
  y = odeRealToComplexState(y, n);
  if isImplicit
    yp = odeRealToComplexState(varargin{1}, n);
    args = [{t, y, yp}, varargin(2:end)];
  else
    args = [{t, y}, varargin];
  end
  dy = feval(fcn, args{:});
  dy = odeComplexToRealState(dy);
end
%=============================================================================
function stop = odeSeparatedComplexOutputFcn(fcn, n, outputSel, t, y, flag, varargin)
  if ~isempty(y)
    y = odeRealToComplexState(y, n);
  end
  if ~isempty(outputSel) && isempty(flag)
    y = y(outputSel, :);
  end
  stop = feval(fcn, t, y, flag, varargin{:});
end
%=============================================================================
function varargout = odeSeparatedComplexEventFcn(fcn, n, t, y, varargin)
  y = odeRealToComplexState(y, n);
  args = {t, y};
  if ~isempty(varargin)
    rest = varargin;
    first = rest{1};
    if isnumeric(first) && size(first, 1) == 2 * n
      rest{1} = odeRealToComplexState(first, n);
    end
    args = [args, rest];
  end
  nin = nargin(fcn);
  if nin >= 0 && nin < length(args)
    args = args(1:nin);
  end
  if nargout == 0
    feval(fcn, args{:});
  else
    [varargout{1:nargout}] = feval(fcn, args{:});
  end
end
%=============================================================================
function varargout = odeSeparatedComplexCallback(fcn, n, t, y, index, varargin)
  y = odeRealToComplexState(y, n);
  nout = nargout;
  if nout <= 0
    feval(fcn, t, y, index, varargin{:});
    varargout = {};
  elseif nout == 1
    varargout{1} = feval(fcn, t, y, index, varargin{:});
  else
    [varargout{1}, eventY] = feval(fcn, t, y, index, varargin{:});
    varargout{2} = odeComplexToRealState(eventY);
  end
end
%=============================================================================
function z = odeComplexToRealState(y)
  y = y(:);
  z = [real(y); imag(y)];
end
%=============================================================================
function y = odeRealToComplexState(z, n)
  y = z(1:n, :) + 1i * z((n + 1):(2 * n), :);
end
%=============================================================================
function block = odeComplexMatrixBlock(value, n)
  if isscalar(value)
    if isreal(value)
      block = value;
    else
      eyeN = eye(n);
      block = [real(value) * eyeN -imag(value) * eyeN; ...
        imag(value) * eyeN real(value) * eyeN];
    end
    return
  end
  block = [real(value) -imag(value); imag(value) real(value)];
end
%=============================================================================
