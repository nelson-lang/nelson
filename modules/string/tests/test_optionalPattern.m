%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
p = optionalPattern('s');
asserts.istrue(isa(p, 'pattern'));
asserts.isequal(extract(["cat"; "cats"], "cat" + p), ["cat"; "cats"]);
%=============================================================================
