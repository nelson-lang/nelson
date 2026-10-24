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
[status, output] = run_nelson_cli(nelson_cli, ' --quiet --timeout abc -e "quit"');
asserts.notEqual(status, 0);
asserts.istrue(contains(output, 'Nelson:ProgramOptions:InvalidTimeout'));
asserts.istrue(contains(output, 'wrong value for timeout option'));
%=============================================================================
[status, output] = run_nelson_cli(nelson_cli, ' --quiet --timeout -e "quit"');
asserts.notEqual(status, 0);
asserts.istrue(contains(output, 'Nelson:ProgramOptions:MissingValue'));
asserts.istrue(contains(output, 'no value after --timeout'));
%=============================================================================
[status, output] = run_nelson_cli(nelson_cli, ' --quiet --timeout= -e "quit"');
asserts.notEqual(status, 0);
asserts.istrue(contains(output, 'no value after --timeout'));
%=============================================================================
