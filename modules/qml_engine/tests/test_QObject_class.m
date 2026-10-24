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
qml_file_ok = [modulepath('qml_engine', 'tests'), '/test_qml_loadfile_window_qt6.qml'];
qobj1 = qml_loadfile(qml_file_ok);
qobj2 = qml_loadfile(qml_file_ok);
qobj3 = qml_loadfile(qml_file_ok);
%=============================================================================
A = [qobj1, qobj2, qobj3];
%=============================================================================
asserts.isequal(class(A), 'QObject');
%=============================================================================
delete(qobj1);
asserts.isequal(class(A), 'QObject');
asserts.isequal(class(qobj1), 'QObject');
asserts.isfalse(isvalid(qobj1));
%=============================================================================
delete(qobj2);
asserts.isequal(class(A), 'QObject');
asserts.isequal(class(qobj2), 'QObject');
asserts.isfalse(isvalid(qobj2));
%=============================================================================
delete(qobj3);
asserts.isequal(class(A), 'QObject');
asserts.isequal(class(qobj3), 'QObject');
asserts.isfalse(isvalid(qobj3));
%=============================================================================
