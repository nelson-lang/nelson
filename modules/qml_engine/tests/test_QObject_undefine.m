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
assert_isequal(nargin('QObject_undefine'), 2);
assert_isequal(nargout('QObject_undefine'), 0);
%=============================================================================
if semver(qt_version(), '>=6.0')
  qml_file = [modulepath('qml_engine', 'tests'), '/test_qvariant_qt6.qml'];
else
  qml_file = [modulepath('qml_engine', 'tests'), '/test_qvariant_qt5.qml'];
end
qobj = qml_loadfile(qml_file);
%=============================================================================
qobj.toto = 1;
assert_isequal(qobj.toto, 1);
assert_checkerror('qobj.toto = ''1'';', _('Expected a real value scalar.'));
QObject_undefine(qobj, 'toto');
qobj.toto = '1';
assert_isequal(qobj.toto, '1');

%=============================================================================

