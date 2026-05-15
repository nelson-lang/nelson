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
      error(_('Invalid input argument: title text expected.'));
    end
    parent = varargin{1};
    titleContent = varargin{2};
    if isStringScalar(titleContent)
      titleContent = convertStringsToChars(titleContent);
    end
    h = parent.Title;
    s = parent.Subtitle;
    indexArguments = 3;
    hasSubtitle = false;
    if (indexArguments <= nargin && (ischar(varargin{indexArguments}) || isStringScalar(varargin{indexArguments})))
      candidatePropertyName = varargin{indexArguments};
      if isStringScalar(candidatePropertyName)
        candidatePropertyName = convertStringsToChars(candidatePropertyName);
      end
      if (indexArguments == nargin || ~isprop(h, candidatePropertyName))
        hasSubtitle = true;
        subtitleContent = varargin{indexArguments};
        if isStringScalar(subtitleContent)
          subtitleContent = convertStringsToChars(subtitleContent);
        end
        indexArguments = indexArguments + 1;
      end
    end
    if indexArguments <= nargin
      propertyArguments = varargin(indexArguments:end);
    else
      propertyArguments = {};
    end
    set(h, 'String', titleContent, ...
    'HorizontalAlignment','center', ...
    'VerticalAlignment','bottom', ...
    'Position', [0.5, 1, 0], ...
    'FontWeight', 'bold', ...
    'Margin', 3, ...
    propertyArguments{:});
    if hasSubtitle
      set(s, 'String', subtitleContent, ...
      'HorizontalAlignment','center', ...
      'VerticalAlignment','top', ...
      'Position', [0.5, 0.97, 0], ...
      'FontWeight', 'normal', ...
      'Margin', 3);
    end
    if (nargout > 0)
      varargout{1} = h;
    end
    if (nargout > 1)
      varargout{2} = s;
    end
    return;
  elseif isgraphics(varargin{1})
    if nargout > 1
      error(_('Too many output arguments.'));
    end
    indexArguments = 2;
    parent = varargin{1};
  elseif (ischar(varargin{1}) || isStringScalar(varargin{1}))
    if nargout > 1
      error(_('Too many output arguments.'));
    end
    indexArguments = 1;
    parent = gca();
  else
    error(_('Invalid input argument: graphics object or text value expected.'));
  end
  
  saveca = gca();
  axes(parent);
  visibility = parent.Visible;
  
  titleContent = varargin{indexArguments};
  if isStringScalar(titleContent)
    titleContent = convertStringsToChars(titleContent);
  end
  
  indexArguments = indexArguments + 1;
  parent.Title = __text__('String', titleContent, ...
  'HorizontalAlignment','center', ...
  'VerticalAlignment','top', ...
  'Position', [0.5, 1, 0], ...
  'Parent', parent, 'AutoParent', 'off', ...
  'FontWeight', 'bold', ...
  'Margin', 3, ...
  'Visible', visibility, ...
  varargin{indexArguments:end});
  
  axes(saveca);
  
  if (nargout > 0)
    varargout{1} = parent.Title;
  end
end
%=============================================================================
