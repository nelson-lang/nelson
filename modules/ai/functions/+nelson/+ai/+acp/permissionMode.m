%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Global file-access permission mode for the ACP assistant (session-wide), read
% before deciding a session/request_permission:
%   'always_ask' - prompt on every request (the default, safest).
%   'auto'       - auto-allow safe requests, prompt only for risky ones.
%   'full'       - never prompt, auto-allow everything.
% permissionMode() returns the current mode; permissionMode(mode) sets it (an
% unrecognized value is ignored) and returns the effective mode. The choice is
% remembered across restarts in a small preferences file, like the compiler
% configuration, so the user sets it once instead of every session.
function out = permissionMode(mode)
  narginchk(0, 1);
  nargoutchk(0, 1);
  persistent current
  if isempty(current)
    current = loadPermissionMode();
  end
  if nargin >= 1 && ~isempty(mode)
    candidate = lower(char(mode));
    if any(strcmp(candidate, {'always_ask', 'auto', 'full'}))
      current = candidate;
      savePermissionMode(current);
    end
  end
  out = current;
end
%=============================================================================
% Read the stored mode; fall back to the safe default on any missing or
% malformed file, so a corrupted preference never breaks the assistant.
function mode = loadPermissionMode()
  mode = 'always_ask';
  jsonfile = permissionModeFile();
  if isfile(jsonfile)
    try
      data = jsondecode(jsonfile, '-file');
    catch
      return
    end
    if isstruct(data) && isfield(data, 'mode')
      candidate = lower(char(data.mode));
      if any(strcmp(candidate, {'always_ask', 'auto', 'full'}))
        mode = candidate;
      end
    end
  end
end
%=============================================================================
% Persist the mode; a write failure (read-only prefs dir) is swallowed so the
% selector still works for the running session.
function savePermissionMode(mode)
  try
    dir = [prefdir(), '/assistant'];
    if ~isfolder(dir)
      mkdir(dir);
    end
    filewrite(permissionModeFile(), jsonencode(struct('mode', mode)));
  catch
  end
end
%=============================================================================
function f = permissionModeFile()
  f = [prefdir(), '/assistant/permission_mode.json'];
end
%=============================================================================
