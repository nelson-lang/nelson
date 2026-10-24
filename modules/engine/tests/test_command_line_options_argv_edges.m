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
[status, output] = run_nelson_cli(nelson_cli, ' --quiet --no-user-startup --no-ipc -e "try, a = argv(''user''); expected = [char(39), ''literal quote'', char(39)]; assert(isequal(a, {expected})); exit(0); catch, exit(1); end" -- "''literal quote''"');
asserts.isequal(status, 0);
%=============================================================================
