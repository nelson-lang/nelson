%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% the diagonal of an empty complex array is real
asserts.istrue(isreal(diag(complex([]))));
asserts.istrue(isreal(diag(complex(zeros(0, 1)))));
asserts.istrue(isreal(diag(complex(zeros(0, 3)))));
asserts.isfalse(isreal(diag([1i, 2i])));
%=============================================================================
