%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
have_xmldoc = isfile([modulepath('help_tools'), '/help/en_US/xml/chapter.xml'])
skip_testsuite(~have_xmldoc, 'XML documentation is missing')
%=============================================================================
[a, b, c] = xmldocchecker([modulepath('help_tools'), '/help/en_US/xml/chapter.xml']);
assert_istrue(a);
assert_istrue(isempty(b));
assert_istrue(isempty(c));
%=============================================================================
assert_istrue(xmldocchecker());
%=============================================================================  
