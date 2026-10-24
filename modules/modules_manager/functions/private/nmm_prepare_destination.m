%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function nmm_prepare_destination(destination_dir, module_name, module_version)
  if nmm_is_installed(module_name, module_version)
    nmm_error('DestinationExists', 'Destination already exists: %s', destination_dir);
  end
  if ~isSafeOrphanDestination(destination_dir, module_name, module_version)
    nmm_error('DestinationExists', 'Destination already exists: %s', destination_dir);
  end
  [r, msg] = rmdir(destination_dir, 's');
  if ~r
    nmm_error('DestinationRemoveFailed', msg);
  end
end
%=============================================================================
function tf = isSafeOrphanDestination(destination_dir, module_name, module_version)
  tf = false;
  root = usermodulesdir();
  if ~startsWith(destination_dir, root)
    return
  end
  moduleJson = [destination_dir, 'module.json'];
  if ~isfile(moduleJson)
    return
  end
  try
    info = jsondecode(moduleJson, '-file');
    tf = isfield(info, 'module') && isfield(info, 'version') ...
        && strcmp(info.module, module_name) && strcmp(info.version, module_version);
  catch
  end
end
%=============================================================================
