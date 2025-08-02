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
assert_isequal(nargin('qml_addimportpath'), 1);
assert_isequal(nargout('qml_addimportpath'), 0);
%=============================================================================
l1 = qml_importpathlist();
qml_addimportpath(nelsonroot());
l2 = qml_importpathlist();
assert_istrue(length(l2) > length(l1))
for k = l2
  assert_istrue(length(k) > 0);
end
%=============================================================================
