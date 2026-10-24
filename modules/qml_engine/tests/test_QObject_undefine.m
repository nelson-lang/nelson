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
asserts.isequal(nargin('QObject_undefine'), 2);
asserts.isequal(nargout('QObject_undefine'), 0);
%=============================================================================
qml_file = [modulepath('qml_engine', 'tests'), '/test_qvariant_qt6.qml'];
qobj = qml_loadfile(qml_file);
%=============================================================================
qobj.toto = 1;
asserts.isequal(qobj.toto, 1);
asserts.checkerror('qobj.toto = ''1'';', _('A scalar expected.'));
QObject_undefine(qobj, 'toto');
qobj.toto = '1';
asserts.isequal(qobj.toto, '1');

%=============================================================================
