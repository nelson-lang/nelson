%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
% <--WITH DISPLAY-->
% <--NATIVE ARCHITECTURE TEST REQUIRED-->
% <--INTERACTIVE TEST-->
%=============================================================================
if ispc()
  qml_file = [modulepath('qml_engine'), '/examples/dynamic_scene_demo/dynamicscene.qml'];
  qobj = qml_createqquickview(qml_file);
  qobj.title = 'Nelson demo dynamic scene';
end
%=============================================================================
