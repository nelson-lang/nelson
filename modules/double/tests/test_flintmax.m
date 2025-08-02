%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(flintmax(), 2^53);
assert_isequal(flintmax('double'), 2^53);
assert_isequal(flintmax('single'), single(2^24));
%=============================================================================
