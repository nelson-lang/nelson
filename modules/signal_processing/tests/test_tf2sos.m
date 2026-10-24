%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
[b, a] = butter(2, 0.4);
sos = tf2sos(b, a);
asserts.isequal(size(sos, 2), 6);
[bt, at] = sos2tf(sos);
asserts.isapprox(bt ./ bt(1), b ./ b(1), 1e-8);
asserts.isapprox(at ./ at(1), a ./ a(1), 1e-8);
%=============================================================================
