%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
n = 5000;
y = 2020 + mod(0:n - 1, 5);
mo = 1 + mod(0:n - 1, 12);
d = 1 + mod(0:n - 1, 28);
tic();
serial = datenum(y, mo, d);
[Y, M, D] = datevec(serial);
t = datetime(Y, M, D);
r = yyyymmdd(t);
shifted = dateshift(t, 'start', 'month');
delta = days(1:n);
toc();
asserts.isequal(length(serial), n);
asserts.isequal(length(r), n);
asserts.isequal(length(shifted), n);
asserts.isequal(length(delta), n);
%=============================================================================
