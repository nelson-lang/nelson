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
asserts.isequal(nargin('QObject_iswindowtype'), 1);
asserts.isequal(nargout('QObject_iswindowtype'), 1);
%=============================================================================
qml_file_ok = [modulepath('qml_engine', 'tests'), '/test_qml_loadfile_window_qt6.qml'];
qobj = qml_loadfile(qml_file_ok);
asserts.istrue(QObject_iswindowtype(qobj));
child = qobj.children(1);
asserts.isfalse(QObject_iswindowtype(child));
%=============================================================================
delete(qobj);
%=============================================================================
