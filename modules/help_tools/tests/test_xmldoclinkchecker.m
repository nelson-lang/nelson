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
asserts.istrue(state);
asserts.istrue(isempty(errors_detected));
asserts.istrue(isempty(warnings_detected));
%=============================================================================
[state, errors_detected, warnings_detected] = xmldoclinkchecker([broken_root, '/broken_page.xml']);
asserts.isfalse(state);
asserts.istrue(~isempty(errors_detected));
asserts.istrue(isempty(warnings_detected));
asserts.istrue(~isempty(strfind(errors_detected{1}, 'missing_target')));
%=============================================================================
[state, errors_detected] = xmldoclinkchecker(broken_root);
asserts.isfalse(state);
asserts.istrue(length(errors_detected) == 1);
