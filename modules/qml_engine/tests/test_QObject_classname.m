%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
% <--WITH DISPLAY-->
%=============================================================================
qml_file = [modulepath('qml_engine'), '/tests/test_qml_loadfile_window.qml'];
qobj1 = qml_loadfile(qml_file);
%=============================================================================
assert_isequal(QObject_classname(qobj1), 'QQuickWindow');
ref_1 = {'QQuickRootItem';'QQuickText'};
ref_2 = 'QQuickText';
assert_istrue(isequal(QObject_classname(qobj1.children), ref_1) || isequal(QObject_classname(qobj1.children), ref_2));
%=============================================================================
res = isequal(qobj1.className, 'QQuickWindow');
assert_istrue(res);
%=============================================================================
