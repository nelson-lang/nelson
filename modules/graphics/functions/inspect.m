%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% inspect            open the property inspector on the current figure
% inspect(obj)       open the property inspector and select obj
% inspect([h1 h2])   select the first handle (common-property view is a
%                    follow-up); other handles stay in the object browser
% inspect -close     close the property inspector
%
% The inspector panel is hosted inside the figure window and is identical on
% the Qt, webview and wasm backends. inspect() only records the request on the
% figure; each backend opens and targets its own panel on the next refresh.
%=============================================================================
function inspect(varargin)
  if nargin > 1
    error('Nelson:graphics:inspect', 'Too many input arguments.');
  end
  if nargin == 0
    f = get(groot, 'CurrentFigure');
    if isempty(f) || ~isgraphics(f, 'figure')
      error('Nelson:graphics:noFigure', 'No current figure to inspect.');
    end
    local_open(f, f);
    return;
  end
  arg = varargin{1};
  if (ischar(arg) || (isstring(arg) && isscalar(arg))) && strcmpi(char(arg), '-close')
    local_close();
    return;
  end
  if isempty(arg) || ~all(isgraphics(arg(:)))
    error('Nelson:graphics:invalidHandle', 'A graphics object is expected.');
  end
  target = arg(1);
  f = local_figure_of(target);
  local_open(f, target);
end
%=============================================================================
function local_open(f, target)
  setappdata(f, '__InspectorTarget__', __web_graphics_handle__(target, 'number'));
  drawnow();
end
%=============================================================================
function local_close()
  figures = get(groot, 'Children');
  for k = 1:numel(figures)
    setappdata(figures(k), '__InspectorClose__', true);
  end
  drawnow();
end
%=============================================================================
function f = local_figure_of(object)
  if isgraphics(object, 'figure')
    f = object;
  else
    f = ancestor(object, 'figure');
  end
  if isempty(f) || ~isgraphics(f, 'figure')
    error('Nelson:graphics:noFigure', 'The object is not attached to a figure.');
  end
end
%=============================================================================
