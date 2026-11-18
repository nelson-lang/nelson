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
% A real agent asks the client for permission before using a tool. Instead of
% blocking (the old hang) or auto-rejecting, the client now STORES the request so
% the UI can decide, then answers with respondToPermission. This drives a mock
% agent that sends session/request_permission and waits for the answer: the
% request must be captured (pendingPermission), then granting it must let the run
% COMPLETE with the mock reporting it was answered.
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
  binpath = modulepath('nelson', 'bin');
  nelson_exe = [binpath, '/nelson-adv-cli'];
  mock = [modulepath('ai', 'tests'), '/acp_mock_agent_permission.m'];
  client = nelson.ai.acp.internal.connect(nelson_exe, '--quiet', '--no-ipc', '--no-user-startup', ...
    '--no-user-modules', '-f', mock);
  asserts.isequal(client.sessionId, 'mock-session-perm');
  client = nelson.ai.acp.internal.send(client, 'edit a file');
  asserts.istrue(client.pending.active);
  % Poll until the permission request is captured (stored, not yet answered).
  captured = false;
  for k = 1:500
    [done, text, client] = nelson.ai.acp.internal.poll(client, []);
    if isfield(client, 'pendingPermission') && ~isempty(client.pendingPermission)
      captured = true;
      break
    end
    if done
      break
    end
    pause(0.02);
  end
  asserts.istrue(captured);
  asserts.istrue(~isempty(client.pendingPermission.reqId));
  asserts.isequal(client.pendingPermission.allowOptionId, 'allow');
  asserts.isequal(client.pendingPermission.rejectOptionId, 'reject');
  % Grant the permission: the agent resumes and the run completes. The pending
  % permission is cleared.
  client = nelson.ai.acp.internal.respondToPermission(client, true, '');
  asserts.istrue(isempty(client.pendingPermission));
  done = false;
  for k = 1:500
    [done, text, client] = nelson.ai.acp.internal.poll(client, []);
    if done
      break
    end
    pause(0.02);
  end
  asserts.istrue(done);
  asserts.istrue(contains(text, 'permission-answered'));
  nelson.ai.acp.internal.close(client);
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
