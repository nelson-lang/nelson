%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Figure state plus the property-inspector channel. Both the Qt figure window
% and the webview/wasm frontend read this on their idle refresh so a
% command-line inspect() opens and targets the inspector uniformly, and so the
% currently selected object's values stay in sync with script or interaction
% changes. The target is consumed once; the open flag persists until
% inspect('-close'). When inspectHandle is given, the selected object's current
% display values are attached (light, display strings only).
%=============================================================================
function s = __web_figure_state_full__(handle, inspectHandle)
  % Figure handles are 0-based, so -1 (not 0) means "no handle / no request".
  if nargin < 2
    inspectHandle = -1;
  end
  s = __web_figure_state__(handle);
  f = __web_graphics_handle__(handle, 'object');
  target = -1;
  if isappdata(f, '__InspectorTarget__')
    target = double(getappdata(f, '__InspectorTarget__'));
    rmappdata(f, '__InspectorTarget__');
  end
  closeRequest = false;
  if isappdata(f, '__InspectorClose__')
    closeRequest = logical(getappdata(f, '__InspectorClose__'));
    rmappdata(f, '__InspectorClose__');
  end
  s.inspector = struct('target', target, 'close', closeRequest);
  if inspectHandle >= 0
    try
      object = __web_graphics_handle__(inspectHandle, 'object');
      if isgraphics(object) && (isequal(object, f) || isequal(ancestor(object, 'figure'), f))
        s.inspectorValues = __graphics_inspector_values__(object);
      end
    catch
    end
  end
end
%=============================================================================
