%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% non-regression: jsondecode used to leak every element of a reduced JSON
% array (~775 bytes per element); 100 decodes below leaked ~775 MB.
% The threshold is generous so the test never flakes on allocator warm-up.
%=============================================================================
numbers = sprintf('%d,', uint8(repmat('x', 1, 10000)));
numbers(end) = [];
txt = ['[', numbers, ']'];
info = memory();
before = info.MemUsedNelson;
for k = 1:100
  r = jsondecode(txt);
end
clear r
info = memory();
growth = info.MemUsedNelson - before;
assert_istrue(growth < 300e6);
%=============================================================================
