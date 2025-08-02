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
assert_isequal(nargin('qml_importpathlist'), 0);
assert_isequal(nargout('qml_importpathlist'), 1);
%=============================================================================
r = qml_pluginpathlist();
assert_isequal(class(r), 'cell');
assert_isfalse(isempty(r));
assert_isequal(class(r{1}), 'char');
%=============================================================================
