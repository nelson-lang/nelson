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
  [status, output] = run_nelson_cli(nelson_cli, ' --quiet --no-ipc /F=dummy.m');
  asserts.notEqual(status, 0);
  asserts.istrue(contains(output, 'Nelson:ProgramOptions:GuiModeRequired'));
  asserts.istrue(contains(output, 'GUI mode required'));
end
%=============================================================================
