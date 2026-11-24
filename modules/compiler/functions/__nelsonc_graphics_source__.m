%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Resolves a logical graphics asset name (e.g. 'gui') to its on-disk source
% folder under the given graphics root. For a development layout the assets sit
% directly under the root. For an installed macOS application bundle they live
% in Contents/Resources, so the logical name is remapped accordingly.
function source = __nelsonc_graphics_source__(root, name)
  source = fullfile(root, name);
  if isfolder(source)
    return;
  end
  % The gui frontend ships under the module tree (modules/<name>/dist), the same
  % convention as nflow_gui and what nelson-gui serves at runtime, rather than
  % next to the library.
  moduleDist = fullfile(nelsonroot(), 'modules', name, 'dist');
  if isfolder(moduleDist)
    source = moduleDist;
    return;
  end
  contents = __nelsonc_bundle_contents__(root);
  if isempty(contents)
    return;
  end
  parts = strsplit(strrep(name, '\', '/'), '/');
  candidate = fullfile(contents, 'Resources', parts{:});
  if isfolder(candidate)
    source = candidate;
  end
end
