%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
fixture_root = [modulepath('help_tools'), '/tests/xml_linkchecker'];
valid_root = [fixture_root, '/valid'];
broken_root = [fixture_root, '/broken'];
%=============================================================================
[state, errors_detected, warnings_detected] = xmldoclinkchecker(valid_root);
assert_istrue(state);
assert_istrue(isempty(errors_detected));
assert_istrue(isempty(warnings_detected));
%=============================================================================
[state, errors_detected, warnings_detected] = xmldoclinkchecker([broken_root, '/broken_page.xml']);
assert_isfalse(state);
assert_istrue(~isempty(errors_detected));
assert_istrue(isempty(warnings_detected));
assert_istrue(~isempty(strfind(errors_detected{1}, 'missing_target')));
%=============================================================================
[state, errors_detected] = xmldoclinkchecker(broken_root);
assert_isfalse(state);
assert_istrue(length(errors_detected) == 1);
