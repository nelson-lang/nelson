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
h = msgbox({'Operation' 'Completed'});
assert_isfalse(h.minimized);
assert_isequal(h.className, 'QMessageBox');
assert_isequal(h.visible, true);
%=============================================================================
