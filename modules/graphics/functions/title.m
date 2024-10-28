%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = title(varargin)
  narginchk(1, 10000);
  nargoutchk(0, 1);
  
  if isgraphics(varargin{1})
    indexArguments = 2;
    parent = varargin{1};
  elseif (ischar(varargin{1}) || isStringScalar(varargin{1}))
    indexArguments = 1;
    parent = gca();
  else
    error(_('Invalid input argument: graphics object or text value expected.'));
  end
  
  saveca = gca();
  axes(parent);
  visibility = parent.Visible;
  
  titleContent = varargin{indexArguments};
  if isStringScalar(indexArguments)
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
