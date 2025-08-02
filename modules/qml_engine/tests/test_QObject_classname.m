%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
% <--WITH DISPLAY-->
%=============================================================================
if semver(qt_version(), '>=6.0')
  qml_file = [modulepath('qml_engine', 'tests'), '/test_qml_loadfile_window_qt6.qml'];
else
  qml_file = [modulepath('qml_engine', 'tests'), '/test_qml_loadfile_window_qt5.qml'];
end
qobj1 = qml_loadfile(qml_file);
%=============================================================================
if semver(qt_version(), '>=6.0')
  assert_isequal(QObject_classname(qobj1), 'QQuickWindowQmlImpl');
else
  assert_isequal(QObject_classname(qobj1), 'QQuickWindow');
end
ref_1 = {'QQuickRootItem';'QQuickText'};
ref_2 = 'QQuickText';
assert_istrue(isequal(QObject_classname(qobj1.children), ref_1) || isequal(QObject_classname(qobj1.children), ref_2));
%=============================================================================
if semver(qt_version(), '>=6.0')
  res = isequal(qobj1.className, 'QQuickWindowQmlImpl');
else 
  res = isequal(qobj1.className, 'QQuickWindow');
end
assert_istrue(res);
%=============================================================================
