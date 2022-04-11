%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
root = QObject_root();
if ~isempty(root)
  qobj = QObject_findchildren(QObject_root(), 'qcharts_demo', true);
else
  qobj = [];
end
if isempty(qobj)
  qml_file = [modulepath('qml_engine'), '/examples/qcharts_demo/QChartGallery.qml'];
  qobj = qml_createqquickview(qml_file)
  qobj.title = 'Nelson <--> qcharts demo';
  %child_text = qobj.children(2).children(5);
  child_text = QObject_findchildren(qobj, 'textField', true);
  child_text.text = 'Nelson with QCharts.js';
else
  qparent = qobj.parent;
  qparent.visible = true;
end
%=============================================================================
