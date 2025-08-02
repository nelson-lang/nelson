%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if isfile([modulepath('help_tools'), '/help/en_US/xml/chapter.xml'])
  destination = [tempdir(), 'test_buildhelpweb'];
  if isdir(destination)
    rmdir(destination, 's');
  end
  mkdir(destination);
  buildhelpweb(destination);
  r = dir([destination, '/*.html'], '-s');
  assert_istrue(length(r) > 600);
end
%=============================================================================
