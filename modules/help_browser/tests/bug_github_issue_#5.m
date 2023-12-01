%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/5
% <-- Short Description -->
% Help browser did not work on some Windows platforms
%=============================================================================
% <--WINDOWS ONLY-->
bin_path = modulepath('nelson', 'bin');
assert_istrue(isdir([bin_path, '/plugins/sqldrivers']));
assert_istrue(isfile([bin_path, '/plugins/sqldrivers/qsqlite.dll']));
assert_istrue(isfile([bin_path, '/plugins/sqldrivers/qsqlodbc.dll']));
assert_istrue(isfile([bin_path, '/plugins/sqldrivers/qsqlpsql.dll']));
%=============================================================================
