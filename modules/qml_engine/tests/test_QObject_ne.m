%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--GUI MODE-->
% <--WITH DISPLAY-->
%=============================================================================
qml_file_ok = [modulepath('qml_engine', 'tests'), '/test_qml_loadfile_window_qt6.qml'];
qobj = qml_loadfile(qml_file_ok);
asserts.isfalse(qobj ~= qobj);
asserts.istrue(qobj ~= QObject_root());
delete(qobj);
%=============================================================================
