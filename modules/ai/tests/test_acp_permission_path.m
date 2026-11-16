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
% Per-path permission memory: when the user answers 'allow_always_path', the
% request's file is remembered on the client so a later request for the SAME file
% resumes without prompting, while other files still prompt. Session-scoped, not
% written to disk. Checks the private helpers, respondToPermission, and the reply.
%=============================================================================
% Private helpers (reached by putting their folder on path).
addpath([modulepath('ai', 'functions'), '/+nelson/+ai/+acp/private']);
asserts.istrue(isempty(acpNormalizePath('   ')));
c = struct();
asserts.istrue(~acpPathRemembered(c, 'D:/x.m'));
c = acpRememberPath(c, 'D:/x.m');
asserts.istrue(acpPathRemembered(c, 'D:/x.m'));
% A second record of the same path does not duplicate it.
c = acpRememberPath(c, 'D:/x.m');
asserts.isequal(numel(c.permissionAllowPaths), 1);
% A different path is not remembered; an empty path is never remembered.
asserts.istrue(~acpPathRemembered(c, 'D:/y.m'));
c0 = acpRememberPath(struct(), '');
asserts.istrue(~acpPathRemembered(c0, ''));
% On Windows the match is case- and separator-insensitive.
if ispc()
  asserts.istrue(acpPathRemembered(c, 'd:\X.M'));
end
%=============================================================================
% respondToPermission records the pending path only when asked to remember it.
pp = struct('reqId', 7, 'allowOptionId', 'allow', 'rejectOptionId', 'reject', ...
  'path', 'D:/foo.m', 'tool', '', 'kind', '', 'title', '', 'announced', false, 'time', now);
client = struct('pendingPermission', pp);
client = nelson.ai.acp.respondToPermission(client, true, '', true);
asserts.istrue(isempty(client.pendingPermission));
asserts.istrue(acpPathRemembered(client, 'D:/foo.m'));
%=============================================================================
% The reply 'allow_always_path' remembers the file; a different file still prompts.
addpath([modulepath('ai', 'functions'), '/+nelson/+ai/private']);
pp2 = struct('reqId', 9, 'allowOptionId', 'allow', 'rejectOptionId', 'reject', ...
  'path', 'D:/bar.m', 'tool', '', 'kind', '', 'title', '', 'announced', true, 'time', now);
client2 = struct('pendingPermission', pp2);
acpAsyncState('set', 'testpath', client2);
nelson.ai.assistantPermissionReply('testpath', 9, 'allow_always_path');
c2 = acpAsyncState('get', 'testpath');
asserts.istrue(isempty(c2.pendingPermission));
asserts.istrue(acpPathRemembered(c2, 'D:/bar.m'));
asserts.istrue(~acpPathRemembered(c2, 'D:/other.m'));
acpAsyncState('reset', 'testpath');
%=============================================================================
