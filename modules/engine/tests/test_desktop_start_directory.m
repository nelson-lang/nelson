%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
binpath = modulepath('nelson', 'bin');
nelson_web = [binpath, '/nelson-webview'];
if ispc()
  nelson_web = [nelson_web, '.exe'];
end
if ~isfile(nelson_web)
  return
end
%=============================================================================
test_root = tempname();
preferences_directory = [test_root, '/preferences'];
fallback_directory = [test_root, '/fallback'];
recent_directory = [test_root, '/recent'];
startup_directory = [test_root, '/startup'];
if isfolder(test_root)
  rmdir(test_root, 's');
end
mkdir(preferences_directory);
mkdir(fallback_directory);
mkdir(recent_directory);
mkdir(startup_directory);
cleanup_directory = onCleanup(@() rmdir(test_root, 's'));
recent_directory = resolved_directory(recent_directory);
fallback_directory = resolved_directory(fallback_directory);
startup_directory = resolved_directory(startup_directory);
%=============================================================================
previous_preferences = getenv('NELSON_PREFERENCES_PATH');
previous_userpath = getenv('NELSON_USERPATH');
cleanup_preferences = onCleanup(@() setenv('NELSON_PREFERENCES_PATH', previous_preferences));
cleanup_userpath = onCleanup(@() setenv('NELSON_USERPATH', previous_userpath));
setenv('NELSON_PREFERENCES_PATH', preferences_directory);
setenv('NELSON_USERPATH', fallback_directory);
%=============================================================================
recent_file = [preferences_directory, '/recent_directories.json'];
recent_data = struct();
recent_data.directories = {recent_directory};
filewrite(recent_file, jsonprettyprint(jsonencode(recent_data)));
quoted_recent = replace(recent_directory, '''', '''''');
[status, output] = run_nelson_cli(nelson_web, ...
  [' --web --quiet --no-user-startup --no-ipc -e "try, assert(strcmp(pwd(), ''', quoted_recent, ''')); exit(0); catch, exit(1); end"'], 120);
asserts.isequal(status, 0, output);
%=============================================================================
rmfile(recent_file);
quoted_fallback = replace(fallback_directory, '''', '''''');
[status, output] = run_nelson_cli(nelson_web, ...
  [' --web --quiet --no-user-startup --no-ipc -e "try, assert(strcmp(pwd(), ''', quoted_fallback, ''')); exit(0); catch, exit(1); end"'], 120);
asserts.isequal(status, 0, output);
%=============================================================================
quoted_startup = replace(startup_directory, '''', '''''');
filewrite([fallback_directory, '/startup.m'], ['cd(''', quoted_startup, ''');']);
[status, output] = run_nelson_cli(nelson_web, ...
  [' --web --quiet --no-ipc -e "try, assert(strcmp(pwd(), ''', quoted_startup, ''')); exit(0); catch, exit(1); end"'], 120);
asserts.isequal(status, 0, output);
%=============================================================================
function resolved = resolved_directory(directory)
  previous = pwd();
  cleanup = onCleanup(@() cd(previous));
  cd(directory);
  resolved = pwd();
end
%=============================================================================
