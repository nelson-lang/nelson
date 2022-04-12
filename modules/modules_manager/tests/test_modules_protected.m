%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--C/C++ COMPILER REQUIRED-->
% <--NO USER MODULES-->
%=============================================================================
builderFile = [nelsonroot(),'/module_skeleton/builder.m'];
if ~isfile(builderFile)
  return
end
%=============================================================================
cd([nelsonroot(), '/module_skeleton/']);
run('builder.m');
run('loader.m');
[modules_loaded, modules_path, modules_version, modules_protected] = getmodules();
assert_isequal(strcmp(modules_loaded(end), 'module_skeleton'), true);
assert_isequal(modules_protected(end), false);
%=============================================================================

