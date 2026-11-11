%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
help_json = [modulepath('help_tools'), '/help/nelson_help_', getlanguage(), '.json'];
skip_testsuite(~isfile(help_json), ['Help JSON index is missing: ', help_json]);
% the keyword list and the pages shown so far are kept between calls: a second lookup gives the same page
t1 = help('disp');
t2 = help('disp');
asserts.isequal(t1, t2);
asserts.istrue(contains(t1, 'disp'));
% a miss answers an empty text without decoding the index and leaves the cache intact
asserts.isequal(help('zzz_unknown_name_for_cache'), '');
asserts.isequal(help('disp'), t1);
%=============================================================================
