%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function root = __nelsonc_graphics_root__(runtime, libraryDirectory)
  root = libraryDirectory;
  if ~ismac() || ~runtime.gui
    return;
  end
  % On an installed macOS application bundle the graphical runtime assets
  % (gui) live under Contents/Resources rather than next to the library, so the
  % root is the bundle Contents folder. Outside a bundle the assets sit next to
  % the library and the library directory is the root.
  contents = __nelsonc_bundle_contents__(libraryDirectory);
  if isempty(contents)
    return;
  end
  if has_assets(contents, runtime)
    root = contents;
    return;
  end
  error('Nelson:nelsonc:nativeMissing:macOSGraphicalRuntimeAssetsMissing', ...
    'Cannot locate the macOS graphical runtime assets for: %s.', libraryDirectory);
end

function found = has_assets(root, runtime)
  found = true;
  for name = __nelsonc_graphics_assets__(runtime)
    if ~isfolder(__nelsonc_graphics_source__(root, name{1})); found = false; return; end
  end
end
