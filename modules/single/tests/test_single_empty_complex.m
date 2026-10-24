%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% converting an empty complex array gives an empty real array
asserts.istrue(isreal(single(complex([]))));
asserts.isequal(class(single(complex([]))), 'single');
asserts.istrue(isreal(single(complex(single(zeros(2, 0))))));
asserts.isequal(size(single(complex(zeros(2, 0)))), [2 0]);
asserts.isfalse(isreal(single(complex(1))));
%=============================================================================
