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
asserts.isequal(nargin('ismethod'), 2);
asserts.isequal(nargout('methods'), 1);
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
r = ismethod(qobj.children(idx), 'myQmlFunction1');
asserts.istrue(r);
r = ismethod(qobj.children(idx), 'myQmlFunction2');
asserts.istrue(r);
r = ismethod(qobj.children(idx), 'myQmlFunction3');
asserts.istrue(r);
r = ismethod(qobj.children(idx), 'myQmlFunction4');
asserts.istrue(r);
r = ismethod(qobj.children(idx), 'myQmlFunction5');
asserts.isfalse(r);
%=============================================================================
delete(qobj);
%=============================================================================
