%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = binscatter(varargin)
  narginchk(2, 1000);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [parent, args] = parseParent(args);
  constructorArgs = parseBinscatterArguments(parent, args);
  h = __binscatter__(constructorArgs{:});
  updateBinscatterAxes(h);
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function [parent, args] = parseParent(args)
  if ~isempty(args) && isscalar(args{1}) && (isgraphics(args{1}, 'axes') || isgraphics(args{1}, 'hggroup'))
    designatedParent = args{1}(1);
    args = args(2:end);
  else
    % Pull the pair out: the parent is handed to the constructor on its own,
    % and 'Parent' is not one of the binscatter properties validated below.
    [designatedParent, args] = graphicsExtractParentProperty(args);
  end
  % The bins are drawn in an axes, so a group designated as target is resolved
  % to the axes holding it, and that axes is the one to start anew.
  if isgraphics(designatedParent, 'hggroup')
    designatedParent = ancestor(designatedParent, 'axes');
  end
  parent = newplotTarget(designatedParent);
end
%=============================================================================
function constructorArgs = parseBinscatterArguments(parent, args)
  if numel(args) < 2 || ~isRealNumericOrLogical(args{1}) || ~isRealNumericOrLogical(args{2})
    error('Nelson:binscatter:numericXAndExpected', 'Numeric X and Y data expected.');
  end
  x = double(args{1}(:).');
  y = double(args{2}(:).');
  if numel(x) ~= numel(y)
    error('Nelson:dimagree:valueValueNumberElements', '%s and %s must have the same number of elements.', 'X', 'Y');
  end
  args = args(3:end);
  constructorArgs = {'Parent', parent, 'XData', x, 'YData', y};
  flags = struct('NumBins', false, 'NumBinsMode', false, ...
    'XLimits', false, 'XLimitsMode', false, 'YLimits', false, 'YLimitsMode', false);
  if ~isempty(args) && isnumeric(args{1}) && ~isPropertyName(args{1})
    constructorArgs = [constructorArgs, {'NumBins', validateNumBins(args{1})}];
    flags.NumBins = true;
    args = args(2:end);
  end
  if mod(length(args), 2) ~= 0
    error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  for k = 1:2:length(args)
    name = canonicalPropertyName(args{k});
    value = args{k + 1};
    [value, flags] = validatePropertyValue(name, value, flags);
    constructorArgs = [constructorArgs, {name, value}];
  end
  validateModeConsistency(flags, constructorArgs);
end
%=============================================================================
function tf = isRealNumericOrLogical(value)
  tf = (isnumeric(value) || islogical(value)) && isreal(value);
end
%=============================================================================
function n = validateNumBins(value)
  if ~isnumeric(value) || ~isreal(value) || ~(isscalar(value) || numel(value) == 2)
    error('Nelson:graphics:invalidNumBins', 'NumBins must be a positive scalar or two-element vector.');
  end
  n = double(value(:).');
  if any(~isfinite(n)) || any(n < 1) || any(floor(n) ~= n)
    error('Nelson:graphics:invalidNumBins', 'NumBins must be a positive scalar or two-element vector.');
  end
  if isscalar(n)
    n = [n n];
  end
end
%=============================================================================
function limits = validateLimits(value, name)
  if ~isnumeric(value) || ~isreal(value) || numel(value) ~= 2
    error('Nelson:graphics:mustBeTwoElementVector', '%s must be a two-element increasing finite vector.', name);
  end
  limits = double(value(:).');
  if any(~isfinite(limits)) || limits(1) >= limits(2)
    error('Nelson:graphics:mustBeTwoElementVector', '%s must be a two-element increasing finite vector.', name);
  end
end
%=============================================================================
function mode = validateMode(value, name)
  if ~(ischar(value) || isStringScalar(value))
    error('Nelson:binscatter:invalidValue', '%s must be ''auto'' or ''manual''.', name);
  end
  mode = lower(char(value));
  if ~strcmp(mode, 'auto') && ~strcmp(mode, 'manual')
    error('Nelson:binscatter:invalidValue', '%s must be ''auto'' or ''manual''.', name);
  end
end
%=============================================================================
function [value, flags] = validatePropertyValue(name, value, flags)
  switch name
    case 'NumBins'
      value = validateNumBins(value);
      flags.NumBins = true;
    case 'NumBinsMode'
      value = validateMode(value, name);
      flags.NumBinsMode = true;
    case 'XLimits'
      value = validateLimits(value, name);
      flags.XLimits = true;
    case 'XLimitsMode'
      value = validateMode(value, name);
      flags.XLimitsMode = true;
    case 'YLimits'
      value = validateLimits(value, name);
      flags.YLimits = true;
    case 'YLimitsMode'
      value = validateMode(value, name);
      flags.YLimitsMode = true;
  end
end
%=============================================================================
function validateModeConsistency(flags, constructorArgs)
  numBinsMode = propertyValue(constructorArgs, 'NumBinsMode', '');
  xLimitsMode = propertyValue(constructorArgs, 'XLimitsMode', '');
  yLimitsMode = propertyValue(constructorArgs, 'YLimitsMode', '');
  if flags.NumBins && strcmp(numBinsMode, 'auto')
    error('Nelson:binscatter:numBinsCannotBeSpecified', 'NumBins cannot be specified when NumBinsMode is ''auto''.');
  end
  if flags.NumBinsMode && strcmp(numBinsMode, 'manual') && ~flags.NumBins
    error('Nelson:binscatter:numBinsRequiredManual', 'NumBins must be specified when NumBinsMode is ''manual''.');
  end
  if flags.XLimits && strcmp(xLimitsMode, 'auto')
    error('Nelson:binscatter:xLimitsCannotBeSpecified', 'XLimits cannot be specified when XLimitsMode is ''auto''.');
  end
  if flags.XLimitsMode && strcmp(xLimitsMode, 'manual') && ~flags.XLimits
    error('Nelson:binscatter:invalidXLimits', 'XLimits must be specified when XLimitsMode is ''manual''.');
  end
  if flags.YLimits && strcmp(yLimitsMode, 'auto')
    error('Nelson:binscatter:yLimitsCannotBeSpecified', 'YLimits cannot be specified when YLimitsMode is ''auto''.');
  end
  if flags.YLimitsMode && strcmp(yLimitsMode, 'manual') && ~flags.YLimits
    error('Nelson:binscatter:invalidYLimits', 'YLimits must be specified when YLimitsMode is ''manual''.');
  end
end
%=============================================================================
function value = propertyValue(args, name, defaultValue)
  value = defaultValue;
  for k = 1:2:length(args)
    if strcmp(args{k}, name)
      value = args{k + 1};
      return
    end
  end
end
%=============================================================================
function name = canonicalPropertyName(value)
  if ~(ischar(value) || isStringScalar(value))
    error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  names = supportedPropertyNames();
  matches = strcmpi(char(value), names);
  if ~any(matches)
    error('Nelson:graphics:unsupportedProperty', 'Unsupported "%s" option.', 'binscatter');
  end
  name = names{find(matches, 1)};
end
%=============================================================================
function tf = isPropertyName(value)
  if ~(ischar(value) || isStringScalar(value))
    tf = false;
    return
  end
  tf = any(strcmpi(char(value), supportedPropertyNames()));
end
%=============================================================================
function names = supportedPropertyNames()
  names = {'NumBins', 'NumBinsMode', 'XLimits', 'XLimitsMode', ...
    'YLimits', 'YLimitsMode', 'ShowEmptyBins', 'FaceAlpha', ...
    'DisplayName', 'Visible', 'HandleVisibility', 'Tag', 'UserData', ...
    'ButtonDownFcn', 'ContextMenu', 'Selected', 'SelectionHighlight', ...
    'HitTest', 'PickableParts', 'BusyAction', 'Interruptible', ...
    'CreateFcn', 'DeleteFcn'};
end
%=============================================================================
function updateBinscatterAxes(h)
  ax = ancestor(h, 'axes');
  if isempty(ax)
    return
  end
  ax.YDir = 'normal';
  ax.Box = 'on';
  values = h.Values;
  if isempty(values)
    maxValue = 1;
  else
    maxValue = max(values(:));
    if ~isfinite(maxValue) || maxValue <= 0
      maxValue = 1;
    end
  end
  ax.CLim = [0 maxValue];
  try
    colormap(ax, sky(256));
  catch
  end
  cb = colorbar(ax);
  cb.Label.String = _('Bin Counts');
end
%=============================================================================
