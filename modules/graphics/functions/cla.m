%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = cla(varargin)
  narginchk(0, 2);
  nargoutchk(0, 1);
  args = varargin;
  doReset = false;
  if (~isempty(args) && (ischar(args{end}) || isstring(args{end})) ...
      && strcmpi(char(args{end}), 'reset'))
    doReset = true;
    args(end) = [];
  end
  if (isempty(args))
    ax = gca();
  else
    ax = args{1};
  end
  if (doReset)
    % 'reset' restores the axes to defaults, which also removes a second y-axis.
    reset(ax);
  end
  % Clearing the axes does not pick anything: CurrentObject reports what the
  % user last clicked. Deleting the children drops it only when it pointed at
  % one of them.
  ax.Children = [];
  % With two y-axes, each side starts cycling from its own ruler colour: the
  % left side from the first ColorOrder entry, the right side from the second.
  baseColorIndex = 1;
  if strcmp(get(ax, 'YYAxis'), 'on') && strcmp(get(ax, 'YAxisLocation'), 'right')
    baseColorIndex = 2;
  end
  ax.ColorOrderIndex = baseColorIndex;
  ax.LineStyleOrderIndex = 1;
  if nargout == 1
    varargout{1} = ax;
  else
    varargout = {};
  end
end
%=============================================================================
