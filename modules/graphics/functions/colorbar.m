%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = colorbar(varargin)
  % TO DO: a dedicated graphics object.
  nargoutchk(0, 1);
  inputArguments = varargin;
  if length(inputArguments) > 0
    if isgraphics(inputArguments{1}, 'axes') 
     ax = inputArguments{1};
     inputArguments = inputArguments(2:end);
    else
      ax = gca();
    end
  else
    ax = gca();
  end  
  cba = findColorBar(ax);
  if (isempty(cba))
    pos = ax.OuterPosition;
    width = 0.1;
    pos(3) = pos(3) - width;
    ax.OuterPosition = pos;
    npos = [pos(1) + pos(3), pos(2), width, pos(4)];
    cba = axes('OuterPosition', npos, ...
    'YAxisLocation', 'right', ...
    'XTick', 'none', ...
    'Tag', 'Colorbar', ...
    'UserData', ax, ...
    'Colormap', ax.Colormap);
  end
  if (length(inputArguments) > 0)
    axes(cba);
    inputArguments{:}
    set(cba, inputArguments{:});
  end
  cmap = colormap(ax);
  N = size(cmap, 1);
  cmap = linspace(N, 0, N)';
  cmap = repmat(cmap, [1, 4]);
  han = __image__('YData', ax.CLim,'CData', cmap);
  axis('tight');
  axes(ax);
  if nargout == 1
    varargout{1} = cba;
  else
    varargout = {};
  end
end 
%=============================================================================
function cba = findColorBar(ax)
  fig = ax.Parent;
  peers = fig.Children;
  for i = 1:numel(peers)
    if (isgraphics(peers(i), 'axes') && (strcmp(get(peers(i), 'Tag'), 'Colorbar')) && (isequal(get(peers(i), 'UserData'), ax)))
      cba = peers(i);
      return;
    end
  end
  cba = [];
end
%=============================================================================
