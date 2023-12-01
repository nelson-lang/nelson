%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--NO USER MODULES-->
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/10
% <-- Short Description -->
% module_skeleton does not build/load
%=============================================================================
builderFile = [nelsonroot(),'/module_skeleton/builder.m'];
if ~isfile(builderFile)
  return
end
%=============================================================================
if ~isfile([nelsonroot(), '/module_skeleton/loader.m'])
  return
end
%=============================================================================
run([nelsonroot(), '/module_skeleton/loader.m'])
%=============================================================================
