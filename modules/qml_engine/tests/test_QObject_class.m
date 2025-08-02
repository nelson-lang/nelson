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
qobj1 = qml_loadfile(qml_file_ok);
qobj2 = qml_loadfile(qml_file_ok);
qobj3 = qml_loadfile(qml_file_ok);
%=============================================================================
A = [qobj1, qobj2, qobj3];
%=============================================================================
assert_isequal(class(A), 'QObject');
%=============================================================================
delete(qobj1);
assert_isequal(class(A), 'QObject');
assert_isequal(class(qobj1), 'handle');
%=============================================================================
delete(qobj2);
assert_isequal(class(A), 'QObject');
assert_isequal(class(qobj2), 'handle');
%=============================================================================
delete(qobj3);
assert_isequal(class(A), 'handle');
assert_isequal(class(qobj3), 'handle');
%=============================================================================
