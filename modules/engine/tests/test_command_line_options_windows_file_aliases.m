%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ispc()
  binpath = modulepath('nelson', 'bin');
  nelson_cli = [binpath, '/nelson-cli'];
  alias_file = [tempdir(), 'test_command_line_options_', createGUID(), '.m'];
  filewrite(alias_file, {'quit'});
  cleanup_alias_file = onCleanup(@() eval(['if isfile(''', alias_file, '''), rmfile(''', alias_file, '''), end'])); %#ok<NASGU>
  [status, output] = run_nelson_cli(nelson_cli, [' --quiet --no-user-startup --no-ipc /f "', alias_file, '"']);
  asserts.isequal(status, 0);
end
%=============================================================================
