%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(reverse('abc'), 'cba');
asserts.isequal(reverse(["abc", "de"]), ["cba", "ed"]);
asserts.isequal(reverse({'abc', 'de'}), {'cba', 'ed'});
asserts.isequal(reverse("你好世界"), "界世好你");
%=============================================================================
