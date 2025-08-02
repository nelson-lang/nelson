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
  qml_file_ok = [modulepath('qml_engine', 'tests'), '/test_qml_loadfile_window_qt6.qml'];
else
  qml_file_ok = [modulepath('qml_engine', 'tests'), '/test_qml_loadfile_window_qt5.qml'];
end
qobj = qml_loadfile(qml_file_ok);
%=============================================================================
child = QObject_findchildren(qobj, 'text1', true);
assert_isequal(class(child), 'QObject');
assert_isequal(size(child), [1 1]);
assert_isequal(child.className, 'QQuickText');
%=============================================================================
delete(qobj);
%=============================================================================
