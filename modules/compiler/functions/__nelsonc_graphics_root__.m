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
  checked = {root};
  if ~ismac() || ~runtime.gui || has_assets(root, runtime)
    return;
  end
  contents = __nelsonc_bundle_contents__(libraryDirectory);
  if ~isempty(contents)
    checked{end + 1} = contents;
    if bundle_has_essentials(contents, runtime)
      root = contents;
      return;
    end
  end
  for k = 1:numel(runtime.libraries)
    item = runtime.libraries(k);
    marker = '/lib/QtCore.framework/';
    source = strrep(item.source, '\', '/');
    position = strfind(source, marker);
    if strcmp(item.filename, 'QtCore') && ~isempty(position)
      cellar = strfind(source, '/Cellar/');
      if ~isempty(cellar)
        candidate = fullfile(source(1:cellar(1) - 1), 'share', 'qt');
        checked{end + 1} = candidate;
        if has_assets(candidate, runtime); root = candidate; return; end
      end
      opt = strfind(source, '/opt/');
      if ~isempty(opt) && opt(end) > 1
        candidate = fullfile(source(1:opt(end) - 1), 'share', 'qt');
        checked{end + 1} = candidate;
        if has_assets(candidate, runtime); root = candidate; return; end
      end
      candidate = fullfile(source(1:position(1) - 1), 'share', 'qt');
      checked{end + 1} = candidate;
      if has_assets(candidate, runtime); root = candidate; return; end
    end
  end
  error('Nelson:nelsonc:nativeMissing:macOSGraphicalRuntimePluginsMissing', ...
    'Cannot locate the macOS graphical runtime plugins in: %s.', strjoin(unique(checked), ', '));
end

function found = has_assets(root, runtime)
  found = true;
  for name = __nelsonc_graphics_assets__(runtime)
    if ~isfolder(__nelsonc_graphics_source__(root, name{1})); found = false; return; end
  end
end

% An installed bundle ships only the Qt plugins it actually needs, so the full
% development asset set is not required. The platform plugin is indispensable
% for any Qt GUI, and qml is required when the qml engine is deployed.
function ok = bundle_has_essentials(contents, runtime)
  ok = isfolder(__nelsonc_graphics_source__(contents, 'plugins/platforms'));
  if ok && any(strcmp(__nelsonc_graphics_assets__(runtime), 'qml'))
    ok = isfolder(__nelsonc_graphics_source__(contents, 'qml'));
  end
end
