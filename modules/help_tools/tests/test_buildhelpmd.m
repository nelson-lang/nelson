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
have_xmldoc = isfile([modulepath('help_tools'), '/help/en_US/xml/chapter.xml'])
skip_testsuite(~have_xmldoc, 'Help XML sources are not available in this build')
%=============================================================================
destination = tempname();
mkdir(destination);
buildhelpmd(destination, 'help_tools');
r = dir([destination, '/*.md'], '-s');
asserts.istrue(length(r) > 10);
asserts.istrue(isfile([destination, '/en/help_tools/SUMMARY.md']));
asserts.istrue(isfile([destination, '/fr/help_tools/SUMMARY.md']));
rmdir(destination, 's');
%=============================================================================
