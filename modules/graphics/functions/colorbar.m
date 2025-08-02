%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = colorbar(varargin)
  % TO DO: (later) a dedicated graphics object.
  nargoutchk(0, 1);
  width = 0.1;
  inputArguments = varargin;
  if length(inputArguments) > 0
    if (isscalar(inputArguments{1}) && (isgraphics(inputArguments{1}, 'axes')))
      ax = inputArguments{1};
      inputArguments = inputArguments(2:end);
    else
      ax = gca();
    end
  else
    ax = gca();
  end  
  cba = findColorBar(ax);
  if isempty(cba)
    createColorBar(ax, cba, width);
  end
  
  if length(inputArguments) == 1
    param = inputArguments{1};
    if ischar(param) && strcmp(param, 'off') == true
      cba =   colorBarOff(ax, cba, width);
    else
      msg = _("'off' expected.");
      error(msg);
    end
  else
    % Name,Value, ...
    if length(inputArguments) > 0
      set(cba, inputArguments{:});
    end
  end
  if nargout == 1
    varargout{1} = cba;
  else
    varargout = {};
  end
end 
%=============================================================================
function createColorBar(ax, cba, width)
  pos = ax.OuterPosition;
  pos(3) = pos(3) - width;
  npos = [pos(1) + pos(3), pos(2), width / 1.2, pos(4)];
  cba = axes('OuterPosition', npos, ...
  'YAxisLocation', 'right', ...
  'XTick', 'none', ...
  'Tag', 'Colorbar', ...
  'UserData', ax, ...
  'Colormap', ax.Colormap);
  pos(3) = pos(3) + width;
  ax.OuterPosition = pos;
  
  cmap = colormap(ax);
  clen = size(cmap, 1);
  cext = ax.CLim;
  cdiff = (cext(2) - cext(1)) / (clen / 2);
  cmin = cext(1) + cdiff;
  cmax = cext(2) - cdiff;
  __image__('Parent', cba, 'XData', [0, 1], 'YData', [cmin, cmax], 'CData', [1 : clen]');
  axis('tight');
  axes(ax);
end 
%=============================================================================
function cba = colorBarOff(ax, cba, width)
  if ~isempty(cba)
    delete(cba)
    pos = ax.OuterPosition;
    %pos(3) = pos(3) + width;
    ax.OuterPosition = pos;
    cba = [];
  end
end
%=============================================================================
function cba = findColorBar(ax)
  fig = ancestor(ax, 'figure');
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
