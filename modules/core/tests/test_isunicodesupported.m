%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
previous = getenv('NELSON_TERM_IS_UNICODE_SUPPORTED');
setenv('NELSON_TERM_IS_UNICODE_SUPPORTED', 'TRUE');
assert_istrue(isunicodesupported())
setenv('NELSON_TERM_IS_UNICODE_SUPPORTED', previous);
%=============================================================================
