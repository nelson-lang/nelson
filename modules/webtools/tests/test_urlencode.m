%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
R = urlencode('http://www+ad.com?doc=a b?c d#e');
REF = 'http%3A%2F%2Fwww%2Bad.com%3Fdoc%3Da+b%3Fc+d%23e';
assert_isequal(R, REF);
%=============================================================================