%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = axtoolbar(varargin)
  % tb = axtoolbar()
  % tb = axtoolbar(target)
  % tb = axtoolbar(..., buttons)
  % tb = axtoolbar(..., name, value, ...)
  % [tb, btns] = axtoolbar(...)
  nargoutchk(0, 2);
  args = varargin;
  if ~isempty(args) && isscalar(args{1}) && isgraphics(args{1}) && isprop(args{1}, 'Toolbar')
    target = args{1};
    args(1) = [];
  else
    target = gca();
  end
  if mod(numel(args), 2) == 1
    checkButtons(args{1});
    args(1) = [];
  end
  tb = target.Toolbar;
  if isempty(tb)
    error('Nelson:graphics:noAxesToolbar', 'The target has no toolbar. Use reset to restore it.');
  end
  if ~isempty(args)
    set(tb, args{:});
  end
  if nargout > 0
    varargout{1} = tb;
  end
  if nargout > 1
    % The figure toolbar of Nelson is shared by all axes: no per-axes buttons.
    varargout{2} = gobjects(0, 0);
  end
end
%=============================================================================
function checkButtons(buttons)
  if ischar(buttons) || isstring(buttons)
    buttons = cellstr(buttons);
  end
  if ~iscellstr(buttons)
    error('Nelson:graphics:invalidToolbarButtons', 'Buttons must be a character vector, a string array or a cell array of character vectors.');
  end
  known = {'default', 'export', 'brush', 'datacursor', 'rotate', 'pan', 'zoom', 'restoreview', 'zoomin', 'zoomout'};
  for k = 1:numel(buttons)
    if ~any(strcmpi(buttons{k}, known))
      error('Nelson:graphics:invalidToolbarButtons', 'Unknown toolbar button: %s.', buttons{k});
    end
  end
end
%=============================================================================
