%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function response = ai_mcp_request(requests, timeout, varargin)
  if nargin < 2
    timeout = 30;
  end
  server_options = '';
  if nargin > 2
    server_options = varargin{1};
  end

  binpath = modulepath('nelson', 'bin');
  if ispc()
    nelson_exe = ['"', binpath, '/nelson-adv-cli', '"'];
  else
    nelson_exe = [binpath, '/nelson-adv-cli'];
  end

  request_file = [tempname(), '.jsonl'];
  output_file = [tempname(), '.out'];
  fid = fopen(request_file, 'wt');
  for k = 1:numel(requests)
    fprintf(fid, '%s\n', requests{k});
  end
  fclose(fid);
  cleanup_request = onCleanup(@() rmfile(request_file));
  cleanup_output = onCleanup(@() rmfile(output_file));

  if isempty(server_options)
    server_command = 'mcpserver; quit';
  else
    server_command = ['mcpserver(''', strrep(server_options, '''', ''''''), '''); quit'];
  end
  command_argument = escape_mcp_command_argument(server_command);
  process_timeout = int2str(max(1, ceil(timeout) + 5));
  cmd = [nelson_exe, ' --quiet --no-ipc --no-user-startup --no-user-modules --timeout ', process_timeout, ...
    ' --execute="', command_argument, '" < "', request_file, '" > "', output_file, '"'];
  [status, command_output] = system(cmd, timeout);

  response.status = status;
  response.command_output = command_output;
  if isfile(output_file)
    response.output = fileread(output_file);
  else
    response.output = '';
  end
end
%=============================================================================
function value = escape_mcp_command_argument(value)
  if ispc()
    value = strrep(value, '"', '\"');
  else
    value = strrep(value, '\', '\\');
    value = strrep(value, '"', '\"');
    value = strrep(value, '$', '\$');
    value = strrep(value, '`', '\`');
  end
end
%=============================================================================
