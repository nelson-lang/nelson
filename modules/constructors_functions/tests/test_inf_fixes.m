%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(inf([]), zeros([0 0], 'double'));
asserts.isequal(inf(true), Inf);
asserts.isequal(inf(2,3,"single"), single(reshape([Inf Inf Inf Inf Inf Inf], [2 3])));
asserts.isequal(inf(zeros(0,1)), zeros([0 0], 'double'));
asserts.isequal(inf(1,[]), zeros([1 0], 'double'));
asserts.isequal(inf("single"), single(Inf));
asserts.isequal(inf(2,"double"), reshape([Inf Inf Inf Inf], [2 2]));
%=============================================================================
