%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function folders = __nelsonc_graphics_assets__(runtime)
  folders = {};
  if ~runtime.gui
    return;
  end
  folders = {'plugins/platforms', 'plugins/styles', 'plugins/imageformats', ...
    'plugins/iconengines', 'plugins/platforminputcontexts'};
  if any(strcmp(runtime.modules, 'qml_engine'))
    folders = [folders, {'qml', 'plugins/networkinformation', 'plugins/tls'}];
  end
  if any(strcmp(runtime.modules, 'webview'))
    folders{end + 1} = 'web_gui';
  end
end
