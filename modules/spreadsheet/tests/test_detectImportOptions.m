%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
csv_filename = [modulepath('spreadsheet'), '/tests/readcell_1.csv'];
options = detectImportOptions(csv_filename);
assert_isequal(options.DataLines, [2 Inf]);
%=============================================================================
csv_filename = [modulepath('spreadsheet'), '/tests/readcell_2.csv'];
options = detectImportOptions(csv_filename);
assert_isequal(options.DataLines, [6 Inf]);
%=============================================================================
csv_filename = [modulepath('spreadsheet'), '/tests/readcell_3.csv'];
options = detectImportOptions(csv_filename);
assert_isequal(options.DataLines, [1 Inf]);
%=============================================================================
