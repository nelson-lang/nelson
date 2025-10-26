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
destination = [tempdir(), 'test_buildhelpweb'];
if isdir(destination)
  rmdir(destination, 's');
end
mkdir(destination);
buildhelpweb(destination);
r = dir([destination, '/*.html'], '-s');
assert_istrue(length(r) > 600);
%=============================================================================
