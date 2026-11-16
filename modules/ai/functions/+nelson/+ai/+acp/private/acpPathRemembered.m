%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% True when a path was previously allowed for this session (see acpRememberPath),
% so poll can resume the agent without prompting again for the same file.
function tf = acpPathRemembered(client, path)
  tf = false;
  norm = acpNormalizePath(path);
  if isempty(norm)
    return
  end
  if ~isfield(client, 'permissionAllowPaths') || ~iscell(client.permissionAllowPaths)
    return
  end
  tf = any(strcmp(client.permissionAllowPaths, norm));
end
%=============================================================================
