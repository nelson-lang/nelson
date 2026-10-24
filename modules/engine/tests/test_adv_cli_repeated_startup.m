%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ~ispc()
  return
end
%=============================================================================
binpath = modulepath('nelson', 'bin');
nelson_adv_cli = [binpath, '/nelson-adv-cli'];
%=============================================================================
test_root = [tempdir(), 'test_adv_cli_repeated_startup_', createGUID()];
mkdir(test_root);
previous_directory = pwd();
cleanup_directory = onCleanup(@() cd(previous_directory));
cleanup_root = onCleanup(@() rmdir(test_root, 's'));
cd(test_root);
%=============================================================================
startup_count = 45;
startup_count_value = getenv('NELSON_ADV_CLI_REPEATED_STARTUP_COUNT');
if ~isempty(startup_count_value)
  parsed_startup_count = str2double(startup_count_value);
  if isfinite(parsed_startup_count) && parsed_startup_count > 0
    startup_count = round(parsed_startup_count);
  end
end
%=============================================================================
command = ['"', replace(nelson_adv_cli, '"', '\"'), '" --quiet --no-user-startup --no-ipc --timeout 10 -e "quit"'];
for k = 1:startup_count
  [status, output] = system(command, 30);
  diagnostic = ['startup #', int2str(k), '/', int2str(startup_count), newline, output];
  asserts.isequal(status, 0, diagnostic);
end
%=============================================================================
