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
% End-to-end of the interactive permission flow as the dock drives it: the step
% loop (acpAsyncStep) captures and announces the agent's permission request, the
% user's reply (assistantPermissionReply) resolves it, and the run then completes.
% The persisted mode must be the prompting default here, so start from a clean
% slate (no pref file) and restore the user file at the end.
prefFile = [prefdir(), '/assistant/permission_mode.json'];
hadPref = isfile(prefFile);
prefBackup = '';
if hadPref
  prefBackup = fileread(prefFile);
  [~] = rmfile(prefFile);
end
ok = true;
errMsg = '';
try
  % acpAsyncStep / acpAsyncState are private, reached by putting their folder on path.
  addpath([modulepath('ai', 'functions'), '/+nelson/+ai/private']);
  binpath = modulepath('nelson', 'bin');
  nelson_exe = [binpath, '/nelson-adv-cli'];
  mock = [modulepath('ai', 'tests'), '/acp_mock_agent_permission.m'];
  client = nelson.ai.acp.connect(nelson_exe, '--quiet', '--no-ipc', '--no-user-startup', ...
    '--no-user-modules', '-f', mock);
  client = nelson.ai.acp.send(client, 'edit a file');
  acpAsyncState('set', 'testui', client);
  % Step until the permission request is captured and announced to the UI.
  reqId = [];
  for k = 1:500
    acpAsyncStep('testui');
    c = acpAsyncState('get', 'testui');
    if isstruct(c) && isfield(c, 'pendingPermission') && ~isempty(c.pendingPermission) ...
        && c.pendingPermission.announced
      reqId = c.pendingPermission.reqId;
      break
    end
    pause(0.02);
  end
  asserts.istrue(~isempty(reqId));
  % The user grants it once: the pending permission clears and the agent resumes.
  nelson.ai.assistantPermissionReply('testui', reqId, 'allow_once');
  c = acpAsyncState('get', 'testui');
  asserts.istrue(isempty(c.pendingPermission));
  % Step until the run completes (acpAsyncStep returns false when done).
  active = true;
  for k = 1:500
    active = acpAsyncStep('testui');
    if ~active
      break
    end
    pause(0.02);
  end
  asserts.istrue(~active);
  c = acpAsyncState('get', 'testui');
  if isstruct(c) && isfield(c, 'proc') && isvalid(c.proc)
    c.proc.terminate();
  end
  acpAsyncState('reset', 'testui');
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
