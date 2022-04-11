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
R1 = QObject_fieldnames(qobj1);
R2 = fieldnames(qobj1);
assert_isequal(R1, R2);
%=============================================================================
REF = { 'active'
'activeFocusItem'
'children'
'className'
'color'
'contentItem'
'contentOrientation'
'data'
'flags'
'height'
'maximumHeight'
'maximumWidth'
'minimumHeight'
'minimumWidth'
'modality'
'objectName'
'opacity'
'parent'
'title'
'visibility'
'visible'
'width'
'x'
'y'};
assert_istrue(any(contains(R1, REF)));
%=============================================================================
