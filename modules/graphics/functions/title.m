%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = title(varargin)
  narginchk(1, 10000);
  nargoutchk(0, 2);

  if (isgraphics(varargin{1}, 'tiledlayout'))
    if nargin < 2
      error('Nelson:title:textExpected', 'Invalid input argument: title text expected.');
    end
    parent = varargin{1};
    titleContent = charScalar(varargin{2});
    h = parent.Title;
    s = parent.Subtitle;
    [hasSubtitle, subtitleContent, propertyArguments] = parseSubtitleAndProperties(h, varargin, 3);
    set(h, 'String', titleContent, ...
      'HorizontalAlignment', 'center', ...
      'VerticalAlignment', 'bottom', ...
      'FontWeight', 'normal', ...
      propertyArguments{:});
    if hasSubtitle
      set(s, 'String', subtitleContent, ...
        'HorizontalAlignment', 'center', ...
        'VerticalAlignment', 'bottom', ...
        'FontWeight', 'normal');
    end
    if (nargout > 0)
      varargout{1} = h;
    end
    if (nargout > 1)
      varargout{2} = s;
    end
    return;
  elseif isgraphics(varargin{1}, 'legend')
    if nargin < 2
      error('Nelson:title:textExpected', 'Invalid input argument: title text expected.');
    end
    if nargout > 1
      error('Nelson:tooManyOutputs', 'Too many output arguments.');
    end
    parent = varargin{1};
    titleContent = charScalar(varargin{2});
    h = parent.Title;
    set(h, 'String', titleContent, varargin{3:end});
    if (nargout > 0)
      varargout{1} = h;
    end
    return;
  elseif isgraphics(varargin{1})
    indexArguments = 2;
    parent = varargin{1};
  elseif (ischar(varargin{1}) || isStringScalar(varargin{1}) ...
      || (isnumeric(varargin{1}) && isempty(varargin{1})))
    % An empty numeric (title([])) clears the title, like title('').
    indexArguments = 1;
    parent = gca();
  else
    error('Nelson:title:invalidInputArgument', 'Invalid input argument: graphics object or text value expected.');
  end

  % Title stays visible independently of the axes 'Visible' state, so a title
  % set on an axes with hidden decorations (e.g. after 'axis off', as done by
  % pie) still renders.
  visibility = 'on';

  titleContent = charScalar(varargin{indexArguments});

  indexArguments = indexArguments + 1;

  h = parent.Title;
  if ~(isscalar(h) && isgraphics(h, 'text'))
    h = __text__('Parent', parent, 'AutoParent', 'off');
  end
  if isEmptyTextString(h.String)
    set(h, ...
      'Parent', parent, ...
      'Margin', 3, ...
      'AffectAutoLimits', 'off', ...
      'Visible', visibility, ...
      'Clipping', 'off', ...
      'HandleVisibility', 'off', ...
      'FontName', 'Helvetica', ...
      'Color', [0.129411764705882, 0.129411764705882, 0.129411764705882]);
  end

  [hasSubtitle, subtitleContent, propertyArguments] ...
    = parseSubtitleAndProperties(h, varargin, indexArguments);
  setIfNotEmpty(h, propertyArguments);
  parent.Title = h;
  removeChild(parent, parent.Title);
  set(h, 'String', titleContent);

  if isprop(parent, 'Subtitle')
    if hasSubtitle
      s = parent.Subtitle;
      if ~(isscalar(s) && isgraphics(s, 'text'))
        s = __text__('Parent', parent, 'AutoParent', 'off');
      end
      if isEmptyTextString(s.String)
        set(s, ...
          'Parent', parent, ...
          'Margin', 3, ...
          'AffectAutoLimits', 'off', ...
          'Visible', visibility, ...
          'Clipping', 'off', ...
          'HandleVisibility', 'off', ...
          'FontName', 'Helvetica', ...
          'Color', [0.129411764705882, 0.129411764705882, 0.129411764705882]);
      end
      setIfNotEmpty(s, propertyArguments);
      parent.Subtitle = s;
      removeChild(parent, parent.Subtitle);
      set(s, 'String', subtitleContent);
    elseif isgraphics(parent.Subtitle, 'text')
      parent.Subtitle.String = '';
    end
  elseif nargout > 1
    error('Nelson:tooManyOutputs', 'Too many output arguments.');
  end
  setIfNotEmpty(h, propertyArguments);
  if hasSubtitle
    setIfNotEmpty(parent.Subtitle, propertyArguments);
  end
  refreshTitleLayout(parent);
  applyImageAxesTitleState(parent);


  if (nargout > 0)
    varargout{1} = parent.Title;
  end
  if (nargout > 1)
    varargout{2} = parent.Subtitle;
  end
end
%=============================================================================
function applyImageAxesTitleState(parent)
  if ~isgraphics(parent, 'axes') || ~is2D(parent)
    return
  end
  children = get(parent, 'Children');
  hasImage = false;
  for k = 1:numel(children)
    if strcmp(get(children(k), 'Type'), 'image')
      hasImage = true;
      break
    end
  end
  if ~hasImage
    return
  end
  parent.Box = 'on';
  parent.ZLim = [-1 1];
  parent.ZTick = [-1 0 1];
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
function value = charScalar(value)
  if isStringScalar(value)
    value = convertStringsToChars(value);
  end
end
%=============================================================================
function tf = isEmptyTextString(value)
  tf = isempty(value) || (iscell(value) && all(cellfun(@isempty, value(:))));
end
%=============================================================================
function [hasSubtitle, subtitleContent, propertyArguments] ...
  = parseSubtitleAndProperties(h, args, indexArguments)
  hasSubtitle = false;
  subtitleContent = '';
  nargs = numel(args);
  if (indexArguments <= nargs ...
      && (ischar(args{indexArguments}) || isStringScalar(args{indexArguments})))
    candidatePropertyName = charScalar(args{indexArguments});
    if (indexArguments == nargs || ~isprop(h, candidatePropertyName))
      hasSubtitle = true;
      subtitleContent = charScalar(args{indexArguments});
      indexArguments = indexArguments + 1;
    end
  end
  if indexArguments <= nargs
    propertyArguments = args(indexArguments:end);
  else
    propertyArguments = {};
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
