%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/14
% <-- Short Description -->
% error(error_struct) did not return an error.
%=============================================================================
res = run([modulepath('error_manager', 'tests'), '/error_script_issue_#14.m'], 'errcatch');
assert_isequal(res, false);
a = lasterror();
assert_isequal(a.message, _('Requested divide operation requires arguments to have correct dimensions.'));
%=============================================================================

