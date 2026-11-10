%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
X = reshape(linspace(0.01, 100, 1000000), 1000, 1000);
tic(); besselj(0, X); toc();
tic(); besselj(2.5, X); toc();
tic(); bessely(1, X); toc();
tic(); besselk(1.3, X); toc();
tic(); besseli(1, X, 1); toc();
Z = complex(X - 50, 0.1 * X - 5);
tic(); besselj(1, Z); toc();
tic(); besselh(0, 1, Z); toc();
tic(); airy(0, Z); toc();
%=============================================================================
