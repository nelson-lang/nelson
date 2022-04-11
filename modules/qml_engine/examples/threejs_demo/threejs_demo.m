%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% QtCanvas3D was deprecated in Qt 5.13
% This example will work only < Qt 5.13
%=============================================================================
root = QObject_root();
if ~isempty(root)
  qobj = QObject_findchildren(QObject_root(), 'threejs_demo', true);
else
  qobj = [];
end
if isempty(qobj)
  qml_file = [modulepath('qml_engine'), '/examples/threejs_demo/main.qml'];
  qobj = qml_loadfile(qml_file);
  qobj.title = 'Nelson - ThreeJS - BufferGeometry - Lines';
else
  qobj.visible = true;
end
%=============================================================================
