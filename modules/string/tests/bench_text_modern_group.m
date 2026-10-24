%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
s = repmat("  name=value  ", 1, 500);
tic(); a = strip(s); toc();
tic(); b = extractAfter(a, "="); toc();
tic(); c = extractBefore(a, "="); toc();
tic(); d = pad(b, 16); toc();
tic(); e = reverse(c); toc();
tic(); f = erase(a, "="); toc();
%=============================================================================
