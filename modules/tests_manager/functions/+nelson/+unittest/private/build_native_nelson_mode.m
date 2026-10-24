%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function [executable, processArguments] = build_native_nelson_mode(exe, command, withMPI)
  nelson_exe_path = getNelsonExecutablePath(exe);
  if withMPI
    executable = getMPIExecutable();
    processArguments = ['-n 2 ', quoteProcessArgument(nelson_exe_path), ' ', strtrim(command)];
  else
    executable = nelson_exe_path;
    processArguments = strtrim(command);
  end
end
