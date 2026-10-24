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
asserts.isequal(nargin('qml_addpluginpath'), 1);
asserts.isequal(nargout('qml_addpluginpath'), 0);
%=============================================================================
l1 = qml_pluginpathlist();
qml_addpluginpath(nelsonroot());
l2 = qml_pluginpathlist();
asserts.istrue(length(l2) > length(l1))
for k = l2(:)'
  asserts.istrue(isdir(k));
end
%=============================================================================
