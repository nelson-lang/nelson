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
Q = [qobj1, qobj2];
delete(Q);
asserts.isfalse(isvalid(Q(1)));
asserts.isfalse(isvalid(Q(2)));
%=============================================================================
