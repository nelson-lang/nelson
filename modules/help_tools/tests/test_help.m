%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
% <--ENGLISH IMPOSED-->
%=============================================================================
help_json = [modulepath('help_tools'), '/help/nelson_help_', getlanguage(), '.json'];
skip_testsuite(~isfile(help_json), ['Help JSON index is missing: ', help_json]);
%=============================================================================
help;
help('help');
help help
txt = help('help');
assert_istrue(ischar(txt));
assert_istrue(contains(txt, 'help function_name'));
assert_istrue(contains(txt, 'help(''function_name'')'));
txt = help('nelson_missing_help_entry');
assert_istrue(ischar(txt));
assert_istrue(isempty(txt));
%=============================================================================
