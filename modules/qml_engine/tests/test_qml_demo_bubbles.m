%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--GUI MODE-->
% <--WITH DISPLAY-->
% <--INTERACTIVE TEST-->
%=============================================================================
if ispc()
  qml_file = [modulepath('qml_engine'), '/examples/d3_demo/bubbles.qml'];
  qobj = qml_createqquickview(qml_file);
end
%=============================================================================
