%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Shared implementation for xline (InterceptAxis 'x') and yline ('y').
function h = constantLineImpl(interceptAxis, args)
  args = convertStringToCharArgs(args);
  [parent, args] = graphicsParseParent(args);
  if isempty(args)
    error('Nelson:graphics:notEnoughInputs', 'Not enough input arguments.');
  end
  value = args{1};
  args(1) = [];
  if ~isnumeric(value) || ~isreal(value)
    error('Nelson:graphics:invalidConstantLineValue', 'Value must be a real numeric scalar or vector.');
  end
  value = double(value(:)');

  propArgs = {};
  labels = {};
  % Optional LineSpec, only immediately after the value.
  if ~isempty(args) && ischar(args{1}) && ~isConstantLineProperty(args{1})
    [lineStyle, colorSpec, markerSpec] = tryLineSpec(args{1});
    if ~isempty(lineStyle) || ~isempty(colorSpec) || ~isempty(markerSpec)
      if ~isempty(colorSpec)
        propArgs = [propArgs, {'Color', colorSpec}];
      end
      if ~isempty(lineStyle)
        propArgs = [propArgs, {'LineStyle', lineStyle}];
      end
      args(1) = [];
      % Optional labels, only after a LineSpec.
      if ~isempty(args) && ~isConstantLineProperty(args{1}) && (ischar(args{1}) || iscellstr(args{1}))
        labels = cellstr(args{1});
        args(1) = [];
      end
    end
  end

  if rem(numel(args), 2) ~= 0
    error('Nelson:graphics:invalidNameValueArguments', 'Invalid parameter/value pair arguments.');
  end

  count = numel(value);
  h = [];
  for k = 1:count
    callArgs = {'InterceptAxis', interceptAxis, 'Value', value(k)};
    if ~isempty(labels)
      callArgs = [callArgs, {'Label', labels{min(k, numel(labels))}}];
    end
    callArgs = [callArgs, propArgs, args];
    if ~isempty(parent)
      callArgs = [{'Parent', parent}, callArgs];
    end
    hk = __constantline__(callArgs{:});
    if isempty(h)
      h = hk;
    else
      h = [h, hk];
    end
  end
end
%=============================================================================
function tf = isConstantLineProperty(name)
  tf = false;
  if ~ischar(name)
    return
  end
  props = {'Value', 'InterceptAxis', 'Label', 'FontAngle', 'FontSize', 'FontName', ...
    'FontWeight', 'Interpreter', 'LabelOrientation', 'LabelHorizontalAlignment', ...
    'LabelVerticalAlignment', 'Alpha', 'LineWidth', 'Color', 'LabelColor', 'LineStyle', ...
    'Layer', 'ColorMode', 'LabelColorMode', 'LineStyleMode', 'Visible', 'HandleVisibility', ...
    'DisplayName', 'ButtonDownFcn', 'BusyAction', 'Interruptible', 'CreateFcn', 'DeleteFcn', ...
    'Tag', 'UserData', 'Selected', 'SelectionHighlight', 'HitTest', 'PickableParts', ...
    'SeriesIndex', 'Parent', 'ContextMenu'};
  tf = any(strcmpi(name, props));
end
%=============================================================================
function [lineStyle, colorSpec, markerSpec] = tryLineSpec(candidate)
  lineStyle = '';
  colorSpec = '';
  markerSpec = '';
  try
    [lineStyle, colorSpec, markerSpec] = colstyle(candidate);
  catch
    lineStyle = '';
    colorSpec = '';
    markerSpec = '';
  end
end
%=============================================================================
