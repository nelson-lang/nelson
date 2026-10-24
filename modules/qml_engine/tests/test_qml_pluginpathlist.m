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
asserts.isequal(nargin('qml_pluginpathlist'), 0);
asserts.isequal(nargout('qml_pluginpathlist'), 1);
%=============================================================================
r = qml_pluginpathlist();
asserts.isequal(class(r), 'cell');
asserts.isfalse(isempty(r));
asserts.isequal(class(r{1}), 'char');
%=============================================================================
