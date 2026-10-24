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
asserts.isequal(nargin('qml_loadfile'), 1);
asserts.isequal(nargout('qml_loadfile'), 1);
%=============================================================================
qml_file_ok = [modulepath('qml_engine', 'tests'), '/test_qml_loadfile_window_qt6.qml'];
qobj = qml_loadfile(qml_file_ok);
asserts.istrue(QObject_iswindowtype(qobj));
delete(qobj);
%=============================================================================
qml_file_ko = [modulepath('qml_engine', 'tests'), '/test_qml_loadfile_rectangle_qt6.qml'];
qobj = qml_loadfile(qml_file_ko);
asserts.isfalse(QObject_iswindowtype(qobj));
asserts.isfalse(QObject_iswidgettype(qobj));
delete(qobj);
%=============================================================================
