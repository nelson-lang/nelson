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
assert_isequal(nargin('isprop'), 2);
assert_isequal(nargout('isprop'), 1);
%=============================================================================
qml_file_ok = [modulepath('qml_engine'), '/tests/test_qml_loadfile_window.qml'];
qobj = qml_loadfile(qml_file_ok);
assert_istrue(isprop(qobj, 'x'));
assert_istrue(isprop(qobj, 'y'));
assert_isfalse(isprop(qobj, 'z'));
qobj.z = 1;
assert_istrue(isprop(qobj, 'z'));
%=============================================================================
delete(qobj);
%=============================================================================
