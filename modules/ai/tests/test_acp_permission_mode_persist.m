%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
% <--NOT FIX FOR WASM-->
%=============================================================================
% The permission mode is remembered across restarts in prefdir. Check that the
% stored value is read on the first call (simulating a new session) and that a
% valid change is written back. Back up the real file and restore it at the end
% so the suite never clobbers the user choice.
prefFile = [prefdir(), '/assistant/permission_mode.json'];
hadPref = isfile(prefFile);
prefBackup = '';
if hadPref
  prefBackup = fileread(prefFile);
  [~] = rmfile(prefFile);
end
prefDir = [prefdir(), '/assistant'];
if ~isfolder(prefDir)
  mkdir(prefDir);
end
ok = true;
errMsg = '';
try
  % A stored mode is loaded on the first call of the session (fresh persistent).
  filewrite(prefFile, jsonencode(struct('mode', 'auto')));
  asserts.isequal(nelson.ai.acp.permissionMode(), 'auto');
  % A valid change is persisted to the file.
  asserts.isequal(nelson.ai.acp.permissionMode('full'), 'full');
  asserts.istrue(isfile(prefFile));
  stored = jsondecode(prefFile, '-file');
  asserts.isequal(stored.mode, 'full');
  % An unrecognized value is ignored and does not change the stored file.
  asserts.isequal(nelson.ai.acp.permissionMode('bogus'), 'full');
  stored = jsondecode(prefFile, '-file');
  asserts.isequal(stored.mode, 'full');
  % A malformed file falls back to the safe default without error.
  filewrite(prefFile, 'not json');
  clear functions % reset the persistent so the next call reloads from disk
  asserts.isequal(nelson.ai.acp.permissionMode(), 'always_ask');
catch e
  ok = false;
  errMsg = e.message;
end
% Restore the user's real preference whatever the outcome above.
if isfile(prefFile)
  [~] = rmfile(prefFile);
end
if hadPref
  filewrite(prefFile, prefBackup);
end
if ~ok
  error(errMsg);
end
%=============================================================================
