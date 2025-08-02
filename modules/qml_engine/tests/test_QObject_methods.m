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
assert_isequal(nargin('methods'), 1);
assert_isequal(nargout('methods'), 1);
%=============================================================================
if semver(qt_version(), '>=6.0')
  qml_file_ok = [modulepath('qml_engine', 'tests'), '/test_qml_methods_qt6.qml'];
else
  qml_file_ok = [modulepath('qml_engine', 'tests'), '/test_qml_methods_qt5.qml'];
end
qobj = qml_loadfile(qml_file_ok);
r = methods(qobj);
assert_istrue(iscellstr(r));
%=============================================================================
if ismethod(qobj.children(2), 'myQmlFunction1')
  idx = 2;
else
  idx = 3;
end
%=============================================================================
r = methods(qobj.children(idx));
ok = 0;
for e = r(:)'
  if strcmp(e, 'myQmlFunction1') == 1 || strcmp(e, 'myQmlFunction2') == 1 || strcmp(e, 'myQmlFunction3') == 1
    ok = ok + 1;
  end
end
assert_isequal(ok, 3);
%=============================================================================
delete(qobj);
%=============================================================================
