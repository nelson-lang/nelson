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
assert_isequal(nargin('qml_setofflinestoragepath'), 1);
assert_isequal(nargout('qml_setofflinestoragepath'), 0);
%=============================================================================
qml_setofflinestoragepath(nelsonroot());
r = qml_offlinestoragepath();
assert_isequal(r, nelsonroot());
%=============================================================================
