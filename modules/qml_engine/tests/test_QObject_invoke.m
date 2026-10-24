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
asserts.isequal(nargin('invoke'), 2);
asserts.isequal(nargout('invoke'), 1);
%=============================================================================
qml_file_ok = [modulepath('qml_engine', 'tests'), '/test_qml_methods_qt6.qml'];
qobj = qml_loadfile(qml_file_ok);
%=============================================================================
if ismethod(qobj.children(2), 'myQmlFunction1')
  idx = 2;
else
  idx = 3;
end
%=============================================================================
R = invoke(qobj.children(idx), 'myQmlFunction1', 1);
REF = {1, int32(1), int32(2), int32(3)};
asserts.isequal(R, REF);
%=============================================================================
R = invoke(qobj.children(idx), 'myQmlFunction1', 'hello');
REF = {'hello', int32(1), int32(2), int32(3)};
asserts.isequal(R, REF);
%=============================================================================
R = invoke(qobj.children(idx), 'myQmlFunction3', 'hello');
REF = [];
asserts.isequal(R, REF);
%=============================================================================
delete(qobj);
%=============================================================================
