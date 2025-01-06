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
%=============================================================================
if semver(qt_version(), '>=6.0')
  qml_file = [modulepath('qml_engine'), '/examples/clock/clocks_qt6.qml'];
else
  qml_file = [modulepath('qml_engine'), '/examples/clock/clocks_qt5.qml'];
end
addpath([modulepath('qml_engine'), '/examples/clock/']);
qobj = qml_loadfile(qml_file);
qobj.visible = true;
%=============================================================================
delete(qobj);
%=============================================================================
