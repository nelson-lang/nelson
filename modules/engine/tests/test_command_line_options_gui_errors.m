%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
binpath = modulepath('nelson', 'bin');
nelson_cli = [binpath, '/nelson-cli'];
%=============================================================================
[status, output] = run_nelson_cli(nelson_cli, ' --quiet --no-ipc --file-ipc dummy.m');
asserts.notEqual(status, 0);
asserts.istrue(contains(output, 'Nelson:ProgramOptions:GuiModeRequired'));
asserts.istrue(contains(output, 'GUI mode required'));
%=============================================================================
[status, output] = run_nelson_cli(nelson_cli, ' --open file1 file2 --quiet --quiet -e "quit"');
asserts.notEqual(status, 0);
asserts.istrue(contains(output, 'multiple option quiet'));
%=============================================================================
[status, output] = run_nelson_cli(nelson_cli, ' --mat a.mat b.mat --timeout abc -e "quit"');
asserts.notEqual(status, 0);
asserts.istrue(contains(output, 'wrong value for timeout option'));
%=============================================================================
[status, output] = run_nelson_cli(nelson_cli, ' --associated-files example.m');
asserts.notEqual(status, 0);
asserts.istrue(contains(output, 'Nelson:ProgramOptions:GuiModeRequired'));
asserts.istrue(contains(output, 'opening associated files requires GUI mode'));
%=============================================================================
