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
[status, output] = run_nelson_cli(nelson_cli, ' --quiet --quiet -e "quit"');
asserts.notEqual(status, 0);
asserts.istrue(contains(output, 'Nelson:ProgramOptions:DuplicateOption'));
asserts.istrue(contains(output, 'multiple option quiet'));
%=============================================================================
[status, output] = run_nelson_cli(nelson_cli, ' --quiet -e "quit" -e "quit"');
asserts.notEqual(status, 0);
asserts.istrue(contains(output, 'multiple option execute'));
%=============================================================================
[status, output] = run_nelson_cli(nelson_cli, ' --quiet --execute="quit" --execute="quit"');
asserts.notEqual(status, 0);
asserts.istrue(contains(output, 'multiple option execute'));
%=============================================================================
[status, output] = run_nelson_cli(nelson_cli, ' --quiet --language en_US --language fr_FR -e "quit"');
asserts.notEqual(status, 0);
asserts.istrue(contains(output, 'multiple option language'));
%=============================================================================
