%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = subtitle(varargin)
  narginchk(1, 10000);
  nargoutchk(0, 1);

  if isSubtitleTarget(varargin{1})
    if nargin < 2
      error('Nelson:subtitle:textExpected', 'Invalid input argument: subtitle text expected.');
    end
    target = varargin{1};
    textValue = varargin{2};
    propertyArguments = varargin(3:end);
  elseif isAnyGraphics(varargin{1})
    error('Nelson:subtitle:invalidInputArgument', 'Invalid input argument: axes or tiledlayout expected.');
  else
    target = gca();
    textValue = varargin{1};
    propertyArguments = varargin(2:end);
  end

  textValue = normalizeSubtitleText(textValue);
  propertyArguments = normalizePropertyArguments(propertyArguments);

  if ~isscalar(target)
    h = gobjects(size(target, 1), size(target, 2));
    for k = 1:numel(target)
      h(k) = setSubtitle(target(k), textValue, propertyArguments);
    end
  else
    h = setSubtitle(target, textValue, propertyArguments);
  end

  if nargout > 0
    varargout{1} = h;
  end
end
%=============================================================================
function h = setSubtitle(target, textValue, propertyArguments)
  if isgraphics(target, 'tiledlayout')
    h = target.Subtitle;
    if isEmptyTextString(h.String)
      set(h, ...
        'HorizontalAlignment', 'center', ...
        'VerticalAlignment', 'bottom', ...
        'FontWeight', 'normal');
    end
    set(h, 'String', textValue, propertyArguments{:});
    return
  end

  if ~(isgraphics(target) && isprop(target, 'Subtitle'))
    error('Nelson:subtitle:invalidInputArgument', 'Invalid input argument: axes or tiledlayout expected.');
  end

  h = target.Subtitle;
  if ~(isscalar(h) && isgraphics(h, 'text'))
    h = __text__('Parent', target, 'AutoParent', 'off');
  end
  if isEmptyTextString(h.String)
    set(h, ...
      'Parent', target, ...
      'Margin', 3, ...
      'AffectAutoLimits', 'off', ...
      'Visible', target.Visible, ...
      'Clipping', 'off', ...
      'HandleVisibility', 'off', ...
      'FontName', 'Helvetica', ...
      'Color', [0.129411764705882, 0.129411764705882, 0.129411764705882]);
  end
  target.Subtitle = h;
  removeChild(target, h);
  set(h, 'String', textValue);
  refreshTitleLayout(target);
  setIfNotEmpty(h, propertyArguments);
end
%=============================================================================
function removeChild(parent, h)
  if ~isgraphics(h)
    return;
  end
  children = allchild(parent);
  if ~isempty(children)
    children(children == h) = [];
    parent.Children = children;
  end
end
%=============================================================================
function tf = isSubtitleTarget(value)
  tf = false;
  if ~isAnyGraphics(value)
    return
  end
  if ~isscalar(value) && ~allSameGraphicsType(value)
    return
  end
  for k = 1:numel(value)
    if ~(isgraphics(value(k), 'tiledlayout') || isprop(value(k), 'Subtitle'))
      return
    end
  end
  tf = true;
end
%=============================================================================
function tf = isAnyGraphics(value)
  try
    tf = isgraphics(value);
  catch
    tf = false;
  end
end
%=============================================================================
function tf = allSameGraphicsType(value)
  tf = true;
  firstType = value(1).Type;
  for k = 2:numel(value)
    if ~strcmp(value(k).Type, firstType)
      tf = false;
      return
    end
  end
end
%=============================================================================
function textValue = normalizeSubtitleText(textValue)
  if ischar(textValue)
    return
  elseif isStringScalar(textValue)
    textValue = convertStringsToChars(textValue);
  elseif isstring(textValue)
    return
  elseif iscellstr(textValue)
    return
  else
    error('Nelson:subtitle:textExpected', 'Invalid input argument: subtitle text expected.');
  end
end
%=============================================================================
function propertyArguments = normalizePropertyArguments(propertyArguments)
  for k = 1:2:numel(propertyArguments)
    if isStringScalar(propertyArguments{k})
      propertyArguments{k} = convertStringsToChars(propertyArguments{k});
    end
  end
end
%=============================================================================
function setIfNotEmpty(h, propertyArguments)
  if ~isempty(propertyArguments)
    set(h, propertyArguments{:});
  end
end
%=============================================================================
function refreshTitleLayout(parent)
  if isprop(parent, 'TitleHorizontalAlignment')
    parent.TitleHorizontalAlignment = parent.TitleHorizontalAlignment;
  end
end
%=============================================================================
function tf = isEmptyTextString(value)
  if iscell(value)
    tf = true;
    for k = 1:numel(value)
      tf = tf && isempty(value{k});
    end
  else
    tf = isempty(value);
  end
end
%=============================================================================
