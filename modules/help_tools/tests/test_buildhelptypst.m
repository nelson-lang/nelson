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
destination = tempname();
mkdir(destination);
buildhelptypst(destination, 'help_tools');
r = dir([destination, '/*.typ'], '-s');
asserts.istrue(length(r) > 10);
asserts.istrue(isfile([destination, '/en/help_tools/main.typ']));
asserts.istrue(isfile([destination, '/en/help_tools/nelson_help.typ']));
asserts.istrue(isfile([destination, '/en/help_tools/buildhelptypst.typ']));
asserts.istrue(isfile([destination, '/fr/help_tools/main.typ']));
asserts.istrue(isfile([destination, '/en/main.typ']));
asserts.istrue(isfile([destination, '/fr/main.typ']));
content = fileread([destination, '/en/main.typ']);
asserts.istrue(contains(content, '#outline(depth: 2)'));
asserts.istrue(contains(content, '#include "help_tools/main.typ"'));
rmdir(destination, 's');
%=============================================================================
