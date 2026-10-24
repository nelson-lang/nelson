%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Property-inspector payload for one object: its full property model plus the
% figure object-browser tree. objectHandle == 0 inspects the figure itself.
% Public so the Qt figure window can evaluate it directly (like
% __web_figure_state_full__), keeping the inspector off the __web_figure_action__
% macro path that interactive animation must not touch.
%=============================================================================
function out = __web_figure_inspect__(handle, objectHandle)
  if nargin < 2
    objectHandle = 0;
  end
  f = __web_graphics_handle__(handle, 'object');
  if ~isgraphics(f, 'figure')
    error('Nelson:graphics:expectedFigure', 'Figure object expected.');
  end
  if objectHandle ~= 0
    object = __web_graphics_handle__(objectHandle, 'object');
    if ~isgraphics(object) || ~(isequal(object, f) || isequal(ancestor(object, 'figure'), f))
      error('Nelson:graphics:invalidTarget', 'Object belongs to another figure.');
    end
  else
    object = f;
  end
  out = struct('model', __graphics_inspector_model__(object), ...
    'tree', __graphics_hierarchy__(f));
end
%=============================================================================
