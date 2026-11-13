%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function outputs = axisLimitsCommand(axisName, nbOutputs, args)
  % Shared body of xlim, ylim and zlim; axisName is 'X', 'Y' or 'Z'.
  limName = [axisName, 'Lim'];
  outputs = {};
  if isempty(args)
    outputs{1} = getAxisLimits(gca(), axisName);
    return
  end
  if numel(args) == 1
    if isgraphics(args{1})
      outputs{1} = getAxisLimits(args{1}, axisName);
      return
    end
    if ischar(args{1}) && ~isKeyword(args{1})
      error('Nelson:graphics:invalidInputArguments', 'Invalid input arguments.');
    end
    ax = gca();
    value = args{1};
  else
    ax = args{1};
    if ~isgraphics(ax, 'axes')
      error('Nelson:graphics:invalidInputArguments', 'Invalid input arguments.');
    end
    value = args{2};
  end
  if ischar(value)
    if strcmp(value, 'mode')
      outputs{1} = ax.([limName, 'Mode']);
      return
    end
    if strcmp(value, 'method')
      outputs{1} = ax.([axisName, 'LimitMethod']);
      return
    end
    if strcmp(value, 'auto') || strcmp(value, 'manual')
      ax.([limName, 'Mode']) = value;
    elseif isLimitMethod(value)
      ax.([axisName, 'LimitMethod']) = value;
      ax.([limName, 'Mode']) = 'auto';
    else
      error('Nelson:graphics:invalidInputArguments', 'Invalid input arguments.');
    end
  elseif isa(value, 'datetime')
    ax.(limName) = datetimeLimitsToSerial(ax, axisName, value);
  elseif isnumeric(value) && numel(value) == 2
    ax.(limName) = value;
  else
    error('Nelson:graphics:invalidInputArguments', 'Invalid input arguments.');
  end
  if nbOutputs == 1
    error('Nelson:graphics:invalidOutputArgument', 'Invalid output argument.');
  end
end
%=============================================================================
function limits = getAxisLimits(ax, axisName)
  % A datetime axis reports its limits as datetime values.
  limits = ax.([axisName, 'Lim']);
  if isgraphics(ax, 'axes') && strcmp(ax.([axisName, 'AxisScaleKind']), 'datetime')
    limits = datetime(limits, 'ConvertFrom', 'datenum');
  end
end
%=============================================================================
function serial = datetimeLimitsToSerial(ax, axisName, value)
  if ~strcmp(ax.([axisName, 'AxisScaleKind']), 'datetime')
    error('Nelson:graphics:datetimeLimitsOnNumericAxis', 'Datetime limits need an axis that displays datetime values.');
  end
  if numel(value) ~= 2 || any(isnat(value(:))) || ~(value(1) < value(2))
    error('Nelson:graphics:invalidDatetimeLimits', 'Limits must be two increasing datetime values.');
  end
  serial = reshape(datenum(value), 1, 2);
end
%=============================================================================
function tf = isKeyword(value)
  tf = any(strcmp(value, {'mode', 'method', 'auto', 'manual'})) || isLimitMethod(value);
end
%=============================================================================
function tf = isLimitMethod(value)
  tf = strcmp(value, 'tickaligned') || strcmp(value, 'tight') || strcmp(value, 'padded');
end
%=============================================================================
