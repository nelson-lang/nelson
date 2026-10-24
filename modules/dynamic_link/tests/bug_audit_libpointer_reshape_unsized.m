%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% LibPointerObject::reshape() now rejects a numericPtr whose original buffer
% size is unknown (_initialDim == -1), so get(p, 'Value') cannot read past the
% end of an unsized external buffer. That guard only triggers for a raw pointer
% returned from a native call (a numericPtr with no recorded size), which cannot
% be constructed from a pure Nelson script, so it has no observable-from-builtin
% regression test. This test only confirms the guard does not break the normal
% sized-libpointer reshape path.
%=============================================================================
b = libpointer('doublePtr', eye(2, 2));
b.reshape(3, 3);
REF = [1 1 0;
  0 0 0;
  0 0 0];
asserts.isequal(get(b, 'Value'), REF);
%=============================================================================
