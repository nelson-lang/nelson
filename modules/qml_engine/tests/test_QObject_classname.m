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
qml_file = [modulepath('qml_engine', 'tests'), '/test_qml_loadfile_window_qt6.qml'];
qobj1 = qml_loadfile(qml_file);
%=============================================================================
asserts.isequal(QObject_classname(qobj1), 'QQuickWindowQmlImpl');
ref_1 = {'QQuickRootItem'; 'QQuickText'};
ref_2 = 'QQuickText';
asserts.istrue(isequal(QObject_classname(qobj1.children), ref_1) || isequal(QObject_classname(qobj1.children), ref_2));
%=============================================================================
res = isequal(qobj1.className, 'QQuickWindowQmlImpl');
asserts.istrue(res);
%=============================================================================
