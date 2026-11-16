%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Record a path the user chose to always allow, so a later request for the same
% file resumes without prompting. Session-scoped: kept on the client, like the
% session-wide "always allow" flag, and never written to disk.
function client = acpRememberPath(client, path)
  norm = acpNormalizePath(path);
  if isempty(norm)
    return
  end
  if ~isfield(client, 'permissionAllowPaths') || ~iscell(client.permissionAllowPaths)
    client.permissionAllowPaths = {};
  end
  if ~any(strcmp(client.permissionAllowPaths, norm))
    client.permissionAllowPaths{end + 1} = norm;
  end
end
%=============================================================================
