%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 16-->
have_xmldoc = isfile([modulepath('help_tools'), '/help/en_US/xml/chapter.xml'])
skip_testsuite(~have_xmldoc, 'Help XML sources are not available in this build')
main_help_archives = dir([nelsonroot(), '/modules/main/help/*.nhz']);
skip_testsuite(isempty(main_help_archives), 'Main help archive is missing')
%=============================================================================
previousHelpModules = getenv('NELSON_HELP_MODULES');
cleanupHelpModules = onCleanup(@() setenv('NELSON_HELP_MODULES', previousHelpModules)); %#ok<NLS0004>
setenv('NELSON_HELP_MODULES', 'help_tools');
destination = tempname();
mkdir(destination);
buildhelpweb(destination);
r = dir([destination, '/*.html'], '-s');
asserts.istrue(length(r) > 10);
asserts.istrue(isfile([destination, '/index.html']));
asserts.istrue(isfile([destination, '/help_tools/help.html']));
rmdir(destination, 's');
%=============================================================================
