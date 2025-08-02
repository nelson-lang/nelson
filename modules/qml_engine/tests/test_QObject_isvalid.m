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
R1 = isvalid(A);
REF = [true, true, true];
assert_isequal(R1, REF);
%=============================================================================
delete(qobj1);
R2 = isvalid(A);
REF = [false, true, true];
assert_isequal(R2, REF);
%=============================================================================
delete(qobj3);
R3 = isvalid(A);
REF = [false, true, false];
assert_isequal(R3, REF);
%=============================================================================
delete(qobj2);
R4 = isvalid(A);
REF = [false, false, false];
assert_isequal(R4, REF);
%=============================================================================
