%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite(~isempty(getenv('CONDA_PREFIX')) && ismac(), ...
  'Skipping xmltransform tests in conda environment on macOS due to libxslt issues.');
%=============================================================================
have_xmldoc = isfile([modulepath('help_tools'), '/help/en_US/xml/chapter.xml'])
skip_testsuite(~have_xmldoc, 'XML documentation is missing')
%=============================================================================
destination = [tempdir(), 'test_buildhelpmd'];
if isdir(destination)
  rmdir(destination, 's');
end
mkdir(destination);
buildhelpmd(destination);
r = dir([destination, '/*.md'], '-s');
assert_istrue(length(r) > 600);
%=============================================================================
