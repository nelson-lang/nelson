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
  % The graphical desktop is the native WebView: a compiled GUI application
  % ships the gui frontend assets and nothing else.
  if any(strcmp(runtime.modules, 'gui'))
    folders{end + 1} = 'gui';
  end
end
