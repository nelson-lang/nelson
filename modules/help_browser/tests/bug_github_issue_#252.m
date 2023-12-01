%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/252
% <-- Short Description -->
% help files of external modules were not loaded.
%=============================================================================
% <--INTERACTIVE TEST-->
% <--GUI MODE-->
% <--C/C++ COMPILER REQUIRED-->
%=============================================================================
builderFile = [nelsonroot(),'/module_skeleton/builder.m'];
if ~isfile(builderFile)
  return
end
%=============================================================================
if ~ismodule('module_skeleton')
  cd([nelsonroot(), '/module_skeleton']);
  run('builder.m');
  run('loader.m');
end
assert_istrue(ismodule('module_skeleton'));
doc();
attributes = helpbrowser('-attributes');
assert_istrue(any(contains(attributes, 'module_skeleton')));
%=============================================================================
