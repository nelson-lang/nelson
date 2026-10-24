%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function cmd = build_command_nelson_mode(exe, command, withMPI)
  persistent nelson_bin_path %#ok<NLS0005>
  if isempty(nelson_bin_path)
    nelson_bin_path = modulepath('nelson', 'bin');
  end
  nelson_exe_path = ['"', nelson_bin_path, '/', exe, '"'];
  cmd = [prefixCommand(withMPI), nelson_exe_path, ' ', command];
end
