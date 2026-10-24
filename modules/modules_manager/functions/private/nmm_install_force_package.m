%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function report = nmm_install_force_package(filename)
  report = nmm_install(filename, '-dry-run');
  wasInstalled = nmm_is_installed(report.module, report.version);
  if wasInstalled
    nmm_uninstall(report.module, report.version, '-force');
  elseif report.destination_exists
    removeOrphanDestination(report.destination);
  end
  nmm_install(filename);
  report.forced = wasInstalled;
end
%=============================================================================
function removeOrphanDestination(destination)
  root = usermodulesdir();
  if ~startsWith(destination, root) || ~isdir(destination)
    return
  end
  [res, msg] = rmdir(destination, 's');
  if ~res
    nmm_error('InstalledDirectoryRemoveFailed', msg);
  end
end
%=============================================================================
