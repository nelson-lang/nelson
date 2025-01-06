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
if semver(qt_version(), '>=6.0')
  qml_file = [modulepath('qml_engine'), '/examples/qcharts_demo/QChartGallery_qt6.qml'];
else
  qml_file = [modulepath('qml_engine'), '/examples/qcharts_demo/QChartGallery_qt5.qml'];
end
qobj = qml_createqquickview(qml_file);
qobj.title = 'Nelson <--> qcharts demo';
%=============================================================================
delete(qobj);
%=============================================================================
