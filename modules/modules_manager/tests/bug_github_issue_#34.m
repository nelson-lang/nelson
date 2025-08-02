%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/34
% <-- Short Description -->
% Windows installer did not copy module_skeleton help files at the good place.
%=============================================================================
builderFile = [nelsonroot(),'/module_skeleton/builder.m'];
if ~isfile(builderFile)
  return
end
%=============================================================================
assert_istrue(isdir([nelsonroot(), '/module_skeleton/help/en_US/xml']));
assert_istrue(isfile([nelsonroot(), '/module_skeleton/help/en_US/xml/nelson_sum.xml']));
assert_istrue(isfile([nelsonroot(), '/module_skeleton/help/en_US/xml/chapter.xml']));
%=============================================================================
