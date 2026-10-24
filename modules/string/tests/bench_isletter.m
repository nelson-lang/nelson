%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
r = blanks(1e8);
r(5) = 't';
tic(); R = isletter(r); toc()
asserts.isequal(R(5), true);
asserts.isequal(R(4), false);