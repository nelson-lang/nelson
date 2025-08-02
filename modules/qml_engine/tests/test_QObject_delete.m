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
h1 = msgbox({'Operation' 'Completed'});
h2 = msgbox({'Operation' 'Completed'});
H = [h1, h2];
delete(H);
assert_isfalse(isvalid(H(1)));
assert_isfalse(isvalid(H(2)));
%=============================================================================
