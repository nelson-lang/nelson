%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
have_xmldoc = isfile([modulepath('help_tools'), '/help/en_US/xml/chapter.xml']);
skip_testsuite(~have_xmldoc, 'Help XML sources are not available in this build')
%=============================================================================
[a, b, c] = xmldocchecker([modulepath('help_tools'), '/help/en_US/xml/chapter.xml']);
asserts.istrue(a);
asserts.istrue(isempty(b));
asserts.istrue(isempty(c));
%=============================================================================
invalid_root = [modulepath('help_tools'), '/tests/xml_invalid'];
[state, errors_detected, warnings_detected] = xmldocchecker(invalid_root);
asserts.isfalse(state);
asserts.istrue(~isempty(errors_detected));
asserts.istrue(contains(errors_detected{1}, 'broken.xml'));
asserts.istrue(contains(errors_detected{1}, 'param_output'));
asserts.istrue(isempty(warnings_detected));
%=============================================================================
asserts.checkerror('xmldocchecker(invalid_root)', errors_detected{1});
%=============================================================================
asserts.istrue(xmldocchecker([modulepath('help_tools'), '/help/en_US/xml']));
%=============================================================================  
