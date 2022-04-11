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
  qobj = QObject_findchildren(QObject_root(), 'dynamic_scene_demo', true);
else
  qobj = [];
end
if isempty(qobj)
  qml_file = [modulepath('qml_engine'), '/examples/dynamic_scene_demo/dynamicscene.qml'];
  qobj = qml_createqquickview(qml_file);
  qobj.title = 'Nelson demo dynamic scene';
else
  qobj.visible = true;
end
%=============================================================================
