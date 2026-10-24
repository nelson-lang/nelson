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
asserts.isequal(nargin('isprop'), 2);
asserts.isequal(nargout('isprop'), 1);
%=============================================================================
qml_file_ok = [modulepath('qml_engine', 'tests'), '/test_qml_loadfile_window_qt6.qml'];
qobj = qml_loadfile(qml_file_ok);
asserts.istrue(isprop(qobj, 'x'));
asserts.istrue(isprop(qobj, 'y'));
if (semver(qt_version(), '6.7.0') < 0)
  asserts.isfalse(isprop(qobj, 'z'));
  qobj.z = 1;
  asserts.istrue(isprop(qobj, 'z'));
else
  if (semver(qt_version(), '6.8.0') < 0)
    asserts.istrue(isprop(qobj, 'z'));
    asserts.isequal(qobj.z, 0);
  else
    asserts.isfalse(isprop(qobj, 'z'));
    qobj.z = 1;
    asserts.istrue(isprop(qobj, 'z'));
  end
end
%=============================================================================
delete(qobj);
%=============================================================================
