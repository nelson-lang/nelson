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
[status, output] = run_nelson_cli(nelson_cli, ' --quiet=true -e "quit"');
asserts.notEqual(status, 0);
asserts.istrue(contains(output, 'Nelson:ProgramOptions:UnexpectedValue'));
asserts.istrue(contains(output, 'option does not take a value --quiet'));
%=============================================================================
[status, output] = run_nelson_cli(nelson_cli, ' --quiet -e');
asserts.notEqual(status, 0);
asserts.istrue(contains(output, 'no value after -e'));
%=============================================================================
[status, output] = run_nelson_cli(nelson_cli, ' --quiet --language -e "quit"');
asserts.notEqual(status, 0);
asserts.istrue(contains(output, 'no value after --language'));
%=============================================================================
[status, output] = run_nelson_cli(nelson_cli, ' --quiet --open --quiet -e "quit"');
asserts.notEqual(status, 0);
asserts.istrue(contains(output, 'no value after --open'));
%=============================================================================
[status, output] = run_nelson_cli(nelson_cli, ' --quiet --mat --timeout 10 -e "quit"');
asserts.notEqual(status, 0);
asserts.istrue(contains(output, 'no value after --mat'));
%=============================================================================
[status, output] = run_nelson_cli(nelson_cli, ' --quiet --associated-files --quiet -e "quit"');
asserts.notEqual(status, 0);
asserts.istrue(contains(output, 'no value after --associated-files'));
%=============================================================================
