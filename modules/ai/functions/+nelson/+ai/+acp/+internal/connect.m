%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Start an ACP agent and perform the initialize and session/new handshake.
% Returns a client struct used by nelson.ai.acp.internal.prompt and nelson.ai.acp.internal.close.
function client = connect(command, varargin)
  narginchk(1, inf);
  timeoutSeconds = 30;
  proc = nelson.os.Process(command, varargin{:});
  if ~isvalid(proc)
    error('Nelson:ai:acpSpawnFailed', 'Unable to start the ACP agent.');
  end
  client = struct();
  client.proc = proc;
  initializeParams = struct('protocolVersion', 1, ...
    'clientInfo', struct('name', 'nelson', 'version', version()));
  client.initialize = acpSendRequest(proc, 1, 'initialize', initializeParams, timeoutSeconds);
  sessionParams = struct('cwd', pwd());
  % Expose Nelson's own MCP server so the agent can look up REAL symbols and help
  % (find_nelson_symbol, get_nelson_help, get_nelson_function_info...) instead of
  % inventing signatures. The server is started read-only (execution and format
  % disabled, see nelsonMcpServers), so only the lookup tools act; the earlier
  % stall came from unanswered tool-permission requests, which the poll now
  % auto-allows for these read-only Nelson tools. ON by default; set
  % NELSON_AI_ACP_MCP=0 (or false/off) to force it OFF.
  if acpMcpEnabled()
    sessionParams.mcpServers = nelsonMcpServers();
  else
    sessionParams.mcpServers = {};
  end
  sessionResult = acpSendRequest(proc, 2, 'session/new', sessionParams, timeoutSeconds);
  if isfield(sessionResult, 'sessionId')
    client.sessionId = sessionResult.sessionId;
  else
    client.sessionId = '';
  end
  client.nextId = 3;
end
%=============================================================================
% Build the ACP mcpServers entry that launches Nelson's own MCP server (a
% separate adv-cli serving JSON-RPC over stdio, scoped to the current working
% directory). Returns an empty cell when the executable cannot be located, so the
% session is created without tools rather than failing.
function servers = nelsonMcpServers()
  servers = {};
  try
    exe = [modulepath('nelson', 'bin'), '/nelson-adv-cli'];
    if ispc()
      exe = [exe, '.exe'];
    end
    if ~isfile(exe)
      return
    end
    % Read-only: disable the execution and format tools so the agent can only
    % query (symbols, help, workspace, lint). This is what makes auto-allowing the
    % permission requests safe and keeps the live session untouched.
    options = ['--workspace-root=', pwd(), ...
      ' --nelson-display-mode=adv-cli --max-output-characters=200000', ...
      ' --allow-execution=false --allow-format=false'];
    args = {'--quiet', '--no-ipc', '--no-user-startup', '--no-user-modules', '-e', ...
      ['nelson.ai.mcp.server(''', options, ''')']};
    servers = {struct('name', 'nelson', 'command', exe, 'args', {args}, 'env', {{}})};
  catch
    servers = {};
  end
end
%=============================================================================
% The Nelson MCP server is attached by default; only an explicit off value in
% NELSON_AI_ACP_MCP disables it (so a build can fall back if a host misbehaves).
function tf = acpMcpEnabled()
  value = lower(strtrim(getenv('NELSON_AI_ACP_MCP')));
  tf = ~any(strcmp(value, {'0', 'false', 'off', 'no'}));
end
%=============================================================================
