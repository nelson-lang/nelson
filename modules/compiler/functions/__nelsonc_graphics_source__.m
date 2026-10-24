%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Resolves a logical graphics asset name (e.g. 'plugins/platforms', 'qml') to
% its on-disk source folder under the given graphics root. For a development
% or Homebrew layout the assets sit directly under the root. For an installed
% macOS application bundle the Qt plugins live in Contents/PlugIns and the
% remaining assets (qml, web_gui) in Contents/Resources, so the logical name
% is remapped accordingly.
function source = __nelsonc_graphics_source__(root, name)
  source = fullfile(root, name);
  if isfolder(source)
    return;
  end
  contents = __nelsonc_bundle_contents__(root);
  if isempty(contents)
    return;
  end
  parts = strsplit(strrep(name, '\', '/'), '/');
  if strcmp(parts{1}, 'plugins') && numel(parts) > 1
    candidate = fullfile(contents, 'PlugIns', parts{2:end});
  else
    candidate = fullfile(contents, 'Resources', parts{:});
  end
  if isfolder(candidate)
    source = candidate;
  end
end
